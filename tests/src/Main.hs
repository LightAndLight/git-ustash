{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import Data.Kind (Type)
import qualified Hedgehog.Range as Range
import System.Directory (getCurrentDirectory, removeDirectoryRecursive, canonicalizePath, doesDirectoryExist, createDirectoryIfMissing, createDirectory)
import Control.Monad.IO.Class (MonadIO)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), makeRelative)
import qualified System.FilePath as FilePath
import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.Generics (Generic)
import Data.Foldable (foldl')
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Morph (hoist)
import Control.Monad (when, guard)
import Control.Exception (finally)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Coerce (coerce)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bifunctor (first)
import Data.Maybe (isNothing)

main :: IO Bool
main = do
  cwd <- getCurrentDirectory
  let testDir = toDir $ cwd </> ".git-ustash-tests"
  checkSequential (Group "Main" [("prop_main", prop_main testDir)])
    `finally` (do
      exists <- doesDirectoryExist $ fromDir testDir
      when exists . removeDirectoryRecursive $ fromDir testDir
    )

prop_main :: Dir -> Property
prop_main testDir = do
  withTests 1000 . property . hoist runResourceT $ do
    actions <- forAll $ Gen.sequential (Range.linear 1 100) (initialState testDir) commands
    
    exists <- evalIO . doesDirectoryExist $ fromDir testDir
    when exists . evalIO . removeDirectoryRecursive $ fromDir testDir
    evalIO . createDirectoryIfMissing True $ fromDir testDir
   
    (ec, stdout, stderr) <- evalIO $ runIn testDir "git" ["init"] ""
    annotateShow stdout
    annotateShow stderr
    evalExitCode ec
    
    executeSequential (initialState testDir) actions
    
commands :: (MonadGen gen, MonadIO m, MonadTest m) => [Command gen m State]
commands =
  [ commandWriteFileUntracked
  , commandWriteFileTracked
  , commandCreateDir
  , commandChangeDir
  , commandGitAdd
  , commandGitCommit
  , commandGitListStaged
  , commandGitListUnstaged
  , commandGitUstashSave
  ]

runIn :: Dir -> String -> [String] -> String -> IO (ExitCode, String, String)
runIn cwd cmd args = 
  readCreateProcessWithExitCode ((proc cmd args){ cwd = Just (fromDir cwd) })

evalExitCode :: (HasCallStack, MonadTest m) => ExitCode -> m ()
evalExitCode e =
  withFrozenCallStack $ case e of
    ExitSuccess -> pure ()
    ExitFailure n -> do
      annotate $ "exit code " <> show n
      failure

newtype Dir = Dir FilePath
  deriving (Eq, Ord, Show)

instance Semigroup Dir where
  (<>) (Dir a) (Dir b) = Dir (a </> b)

toDir :: FilePath -> Dir
toDir path = if last path == '/' then Dir path else Dir (path <> "/")

fromDir :: Dir -> FilePath
fromDir (Dir path) = if last path == '/' then init path else path

splitFileName :: FilePath -> (Dir, FilePath)
splitFileName path = coerce (FilePath.splitFileName path)

data State (v :: Type -> Type) 
  = State
  -- | The temporary directory containing a Git repo.
  { state_root :: Dir
  
  -- | The directory in which Git / @git-ustash@ commands will be run.
  -- Relative to @root@.
  , state_workDir :: Dir
  
  -- | Map from directories to the files they contain.
  , state_files :: Map Dir (Map String File)
   
  -- | The currently staged files.
  , state_staged :: Set FilePath

  -- | Whether or not the Git repo has commits.
  , state_hasCommits :: Bool

  -- | The files currently saved by `git-ustash`
  , state_ustash :: Maybe (Set FilePath)
  }

data File = File {file_contents :: String, file_status :: FileStatus}

data FileStatus 
  = Untracked
  | Tracked Bool
  deriving Eq

initialState :: Dir -> State v
initialState tmpDir =
  State
  { state_root = tmpDir
  , state_workDir = toDir "."
  , state_files = Map.singleton (toDir ".") mempty 
  , state_staged = Set.empty
  , state_ustash = Nothing
  , state_hasCommits = False
  }

data CommandWriteFile (v :: Type -> Type) 
  = CommandWriteFile 
      -- | 'root'
      Dir
      -- | 'workDir'
      Dir
      -- | The path of the file, relative to 'workDir'.
      FilePath
      -- | The contents of the file; lines of text.
      [String]
  deriving (Show, Generic, FunctorB, TraversableB)

state_untrackedFiles :: State v -> [FilePath]
state_untrackedFiles state = do
  (dir, names) <- Map.toList $ state_files state
  (name, file) <- Map.toList names
  guard $ file_status file == Untracked
  pure $ fromDir dir </> name

state_trackedFiles :: State v -> [FilePath]
state_trackedFiles state = do
  (dir, names) <- Map.toList $ state_files state
  (name, file) <- Map.toList names
  guard $ file_status file /= Untracked
  pure $ fromDir dir </> name

commandWriteFileUntracked :: (MonadGen gen, MonadIO m, MonadTest m) => Command gen m State
commandWriteFileUntracked =
  Command 
    (\state ->
        Just $ do
          name <- Gen.string (Range.constant 1 10) Gen.alphaNum
          contents <- Gen.list (Range.constant 0 100) (Gen.string (Range.constant 0 100) Gen.ascii)
          pure $ CommandWriteFile (state_root state) (state_workDir state) name contents
    )
    (\(CommandWriteFile root workDir path content) -> do
      path' <- evalIO $ canonicalizePath (fromDir (root <> workDir) </> path)
      evalIO $ writeFile path' (unlines content)
    )
    [ Require $ \state (CommandWriteFile _ workDir name _) ->
        let dirs = Map.keys $ state_files state in
        (workDir `elem` dirs) &&
        (workDir <> toDir name) `notElem` dirs &&
        (fromDir workDir </> name) `notElem` state_trackedFiles state
    , Update $ \state (CommandWriteFile _ workDir name content) _output ->
        state{ state_files = Map.insertWith (<>) workDir (Map.singleton name $ File (unlines content) Untracked) (state_files state) }
    , Ensure $ \_pre _post _input _output -> do
        label "write file (untracked)"
        
        pure ()
    ]

commandWriteFileTracked :: (MonadGen gen, MonadIO m, MonadTest m) => Command gen m State
commandWriteFileTracked =
  Command 
    (\state -> do
        let trackedFiles = state_trackedFiles state
        guard . not $ null trackedFiles
        Just $ do
          path <- Gen.element trackedFiles
          contents <- Gen.list (Range.constant 0 100) (Gen.string (Range.constant 0 100) Gen.ascii)
          pure $ CommandWriteFile (state_root state) (toDir ".") path contents
    )
    (\(CommandWriteFile root workDir path content) -> do
      path' <- evalIO $ canonicalizePath (fromDir (root <> workDir) </> path)
      evalIO $ writeFile path' (unlines content)
    )
    [ Require $ \state (CommandWriteFile _ workDir name _) ->
        let dirs = Map.keys $ state_files state in
        (workDir `elem` dirs) &&
        (workDir <> toDir name) `notElem` dirs &&
        not (null $ state_trackedFiles state)
    , Update $ \state (CommandWriteFile _ workDir name content) _output ->
        let newContent = unlines content in
        state
          { state_files = 
              Map.adjust 
                (Map.adjust
                  (\(File oldContent oldStatus) -> 
                    File newContent $
                    case oldStatus of
                      Untracked ->
                        undefined
                      Tracked changed ->
                        Tracked $ changed || oldContent /= newContent
                  )
                  name
                )
                workDir
                (state_files state)
          }
    , Ensure $ \_pre _post _input _output -> do
        label "write file (tracked)"
        
        pure ()
    ]

data CommandGitAdd (v :: Type -> Type) 
  = CommandGitAdd
    -- | 'root'
    Dir
    -- | 'workDir'
    Dir
    -- | The files to add, relative to 'root'.
    [FilePath]
    -- | The files to add, relative to 'workDir'.
    [FilePath]
  deriving (Show, Generic, FunctorB, TraversableB)

state_allFiles :: State v -> [(FilePath, File)]
state_allFiles state = do
  (dir, files) <- Map.toList $ state_files state
  fmap (first (fromDir dir </>)) (Map.toList files)

state_dir :: State v -> Dir -> Map FilePath File
state_dir state dir = 
  case Map.lookup dir $ state_files state of
    Nothing ->
      error $ "state_files missing key " <> show dir
    Just a ->
      a

commandGitAdd :: (MonadGen gen, MonadIO m, MonadTest m) => Command gen m State
commandGitAdd =
  Command 
    (\state ->
      let files = fst <$> state_allFiles state in
      if null files
      then Nothing
      else Just $ do
        toAdd <- Gen.subsequence files
        pure $
          CommandGitAdd
            (state_root state)
            (state_workDir state)
            toAdd
            (makeRelative (fromDir $ state_workDir state) <$> toAdd)
    )
    (\(CommandGitAdd root workDir _toAddAbsolute toAddRelative) -> do
      do
        let path = root <> workDir
        annotateShow path
        b <- evalIO . doesDirectoryExist $ fromDir path
        assert b
      
      annotateShow toAddRelative
      (ec, stdout, stderr) <- evalIO $ runIn (root <> workDir) "git" ("add" : toAddRelative) ""
      annotateShow stdout
      annotateShow stderr
      evalExitCode ec
    )
    [ Require $ \state (CommandGitAdd _ _ toAddAbsolute _toAddRelative) ->
        let files = fst <$> state_allFiles state in
        state_workDir state `Map.member` state_files state &&
        all (`elem` files) toAddAbsolute
    , Update $ \state (CommandGitAdd _ _workDir toAddAbsolute _toAddRelative) _output ->
        state{
          state_staged =
            state_staged state 
            `Set.union` 
            Set.fromList
              (filter 
                (\file ->
                  let (dir, name) = splitFileName file in
                  case file_status $ state_dir state dir Map.! name of
                    Tracked changed -> changed
                    Untracked -> True
                )
                toAddAbsolute
              )
        }
    , Ensure $ \_pre _post (CommandGitAdd _root _workDir _toAddAbsolute _toAddRelative) () -> do
        label "git add"
    ]

data CommandGitListStaged (v :: Type -> Type)
  = CommandGitListStaged
      -- | 'root'
      Dir
      -- | 'workDir'
      Dir
  deriving (Show, Generic, FunctorB, TraversableB)

commandGitListStaged :: (MonadGen gen, MonadIO m, MonadTest m) => Command gen m State
commandGitListStaged =
  Command 
    (\state ->
      Just . pure $ CommandGitListStaged (state_root state) (state_workDir state)
    )
    (\(CommandGitListStaged root workDir) -> do
      (ec, stdout, stderr) <- evalIO $ runIn (root <> workDir) "git" ["diff", "--cached", "--name-only"] ""
      annotateShow stderr
      evalExitCode ec
    
      pure . Set.fromList $ ("." </>) <$> lines stdout
    )
    [ Require $ \state (CommandGitListStaged _ workDir) ->
        let dirs = Map.keys (state_files state) in
        (workDir `elem` dirs)
    , Update $ \state _command _output ->
        state
    , Ensure $ \_pre post _input stagedFiles -> do
        label "git list staged files"

        state_staged post === stagedFiles
    ]

data CommandGitListUnstaged (v :: Type -> Type)
  = CommandGitListUnstaged
      -- | 'root'
      Dir
      -- | 'workDir'
      Dir
  deriving (Show, Generic, FunctorB, TraversableB)

state_unstaged :: State v -> Set FilePath
state_unstaged state =
  Set.fromList 
    (fmap fst . 
      filter (\case; (_, File _ (Tracked True)) -> True; _ -> False) $ 
      state_allFiles state
    ) 
    `Set.difference` state_staged state

commandGitListUnstaged :: (MonadGen gen, MonadIO m, MonadTest m) => Command gen m State
commandGitListUnstaged =
  Command 
    (\state ->
      Just . pure $ CommandGitListUnstaged (state_root state) (state_workDir state)
    )
    (\(CommandGitListUnstaged root workDir) -> do
      (ec, stdout, stderr) <- evalIO $ runIn (root <> workDir) "git" ["diff", "--name-only"] ""
      annotateShow stderr
      evalExitCode ec
    
      pure . Set.fromList $ ("." </>) <$> lines stdout
    )
    [ Require $ \state (CommandGitListUnstaged _ workDir) ->
        let dirs = Map.keys (state_files state) in
        (workDir `elem` dirs)
    , Update $ \state _command _output ->
        state
    , Ensure $ \_pre post _input unstagedFiles -> do
        label "git list unstaged files"

        state_unstaged post === unstagedFiles
    ]

data CommandCreateDir (v :: Type -> Type) 
  = CommandCreateDir
      -- | 'root'
      Dir
      -- | 'workDir'
      Dir
      -- | The path of the directory, relative to 'workDir'.
      Dir
  deriving (Show, Generic, FunctorB, TraversableB)

commandCreateDir :: (MonadGen gen, MonadIO m, MonadTest m) => Command gen m State
commandCreateDir =
  Command 
    (\state ->
      Just $ do
        name <- Gen.string (Range.constant 1 10) Gen.alphaNum
        pure $ CommandCreateDir (state_root state) (state_workDir state) (toDir name)
    )
    (\(CommandCreateDir root workDir target) -> do
      evalIO . createDirectory $ fromDir (root <> workDir <> target)
    )
    [ Require $ \state (CommandCreateDir _ workDir path) ->
        let 
          dirs = Map.keys (state_files state)
          files = fst <$> state_allFiles state
        in
        (workDir `elem` dirs) &&
        (workDir <> path) `notElem` dirs &&
        fromDir (workDir <> path) `notElem` files
    , Update $ \state (CommandCreateDir _ workDir path) _output ->
        state{ state_files = Map.insert (workDir <> path) mempty (state_files state) }
    , Ensure $ \_pre _post _input _output -> do
        label "create dir"
        
        pure ()
    ]

data CommandChangeDir (v :: Type -> Type) 
  = CommandChangeDir Dir
  deriving (Show, Generic, FunctorB, TraversableB)

commandChangeDir :: (MonadGen gen, MonadIO m, MonadTest m) => Command gen m State
commandChangeDir =
  Command 
    (\state ->
        Just $ do
          target <- Gen.element $ Map.keys (state_files state)
          pure $ CommandChangeDir target
    )
    (\CommandChangeDir{} -> pure ())
    [ Update $ \state (CommandChangeDir target) _output ->
        state{ state_workDir = target }
    , Ensure $ \_pre _post _input _output -> do
        label "change dir"
        
        pure ()
    ]

data CommandGitCommit (v :: Type -> Type) 
  = CommandGitCommit
      -- | 'root'
      Dir
      -- | 'workDir'
      Dir
      -- | Commit message.
      String
  deriving (Show, Generic, FunctorB, TraversableB)

commandGitCommit :: (MonadGen gen, MonadIO m, MonadTest m) => Command gen m State
commandGitCommit =
  Command 
    (\state ->
        Just $ do
          message <- Gen.string (Range.constant 1 40) Gen.alphaNum
          pure $ CommandGitCommit (state_root state) (state_workDir state) message
    )
    (\(CommandGitCommit root workDir message) -> do
      (ec, stdout, stderr) <- evalIO $ runIn (root <> workDir) "git" ["commit", "-m", message] ""
      annotateShow stdout
      annotateShow stderr
      evalExitCode ec
    )
    [ Require $ \state (CommandGitCommit _ workDir _) -> 
        workDir `Map.member` state_files state &&
        not (null $ state_staged state)
    , Update $ \state (CommandGitCommit _root _workDir _message) _output ->
        let staged = state_staged state in
        state
          { state_staged = Set.empty
          , state_files =
              foldl'
                (\acc stagedFile -> 
                  let (dir, name) = splitFileName stagedFile in
                  Map.adjust (Map.adjust (\file -> file{ file_status = Tracked False }) name) dir acc
                )
                (state_files state)
                staged
          , state_hasCommits = True
          }
    , Ensure $ \_pre _post _input () -> do
        label "git commit"

        pure ()
    ]

data CommandGitUstashSave (v :: Type -> Type) 
  = CommandGitUstashSave
      -- | 'root'
      Dir
      -- | 'workDir'
      Dir
  deriving (Show, Generic, FunctorB, TraversableB)

commandGitUstashSave :: (MonadGen gen, MonadIO m, MonadTest m) => Command gen m State
commandGitUstashSave =
  Command 
    (\state ->
      Just . pure $ CommandGitUstashSave (state_root state) (state_workDir state)
    )
    (\(CommandGitUstashSave root workDir) -> do
      (ec, stdout, stderr) <- evalIO $ runIn (root <> workDir) "git-ustash" ["save"] ""
      annotateShow stdout
      annotateShow stderr
      evalExitCode ec
    )
    [ Require $ \state (CommandGitUstashSave _ workDir) -> 
        workDir `Map.member` state_files state &&
        state_hasCommits state &&
        isNothing (state_ustash state)
    , Update $ \state (CommandGitUstashSave _root _workDir) _output ->
        state { state_ustash = Just (state_unstaged state) }
    , Ensure $ \_pre _post _input () -> do
        label "git-ustash save"

        pure ()
    ]