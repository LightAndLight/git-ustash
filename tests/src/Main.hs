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
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Morph (hoist)
import Control.Monad (when, guard)
import Control.Exception (finally)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Coerce (coerce)
import qualified Data.Set as Set
import Data.Bifunctor (first)
import Data.Maybe (isNothing, fromMaybe, isJust)

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
  , commandReadFile
  , commandCreateDir
  , commandChangeDir
  , commandGitAdd
  , commandGitCommit
  , commandGitListStaged
  , commandGitListUnstaged
  , commandGitListUntracked
  , commandGitUstashSave
  , commandGitUstashRestore
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
  , state_staged :: Map FilePath String

  -- | Whether or not the Git repo has commits.
  , state_hasCommits :: Bool

  -- | The files currently saved by `git-ustash`
  , state_ustash :: Maybe (Map FilePath String)
  }

data File = File {file_contents :: String, file_status :: FileStatus}
  deriving (Eq, Show)

data FileStatus 
  = Untracked
  | Tracked {file_oldContents :: String}
  deriving (Eq, Show)

initialState :: Dir -> State v
initialState tmpDir =
  State
  { state_root = tmpDir
  , state_workDir = toDir "."
  , state_files = Map.singleton (toDir ".") mempty 
  , state_staged = Map.empty
  , state_ustash = Nothing
  , state_hasCommits = False
  }

data CommandReadFile (v :: Type -> Type) 
  = CommandReadFile
      -- | 'root'
      Dir
      -- | The path of the file, relative to 'root'.
      FilePath
  deriving (Show, Generic, FunctorB, TraversableB)

commandReadFile :: (MonadGen gen, MonadIO m, MonadTest m) => Command gen m State
commandReadFile =
  Command 
    (\state -> do
        let allFiles = state_allFiles state
        guard . not $ null allFiles
        Just $ do
          path <- Gen.element $ fmap fst allFiles
          pure $ CommandReadFile (state_root state) path
    )
    (\(CommandReadFile root path) -> do
      path' <- evalIO $ canonicalizePath (fromDir root </> path)
      evalIO $ readFile path'
    )
    [ Require $ \state (CommandReadFile _root path) ->
        path `elem` fmap fst (state_allFiles state)
    , Ensure $ \_pre post (CommandReadFile _root path) output -> do
        label "read file"
        
        output === file_contents (Map.fromList (state_allFiles post) Map.! path)
    ]

state_trackedFiles :: State v -> [FilePath]
state_trackedFiles state = do
  (dir, names) <- Map.toList $ state_files state
  (name, file) <- Map.toList names
  guard $ file_status file /= Untracked
  pure $ fromDir dir </> name

data CommandWriteFileUntracked (v :: Type -> Type) 
  = CommandWriteFileUntracked
      -- | 'root'
      Dir
      -- | 'workDir'
      Dir
      -- | The path of the file, relative to 'workDir'.
      FilePath
      -- | The contents of the file; lines of text.
      [String]
  deriving (Show, Generic, FunctorB, TraversableB)

commandWriteFileUntracked :: (MonadGen gen, MonadIO m, MonadTest m) => Command gen m State
commandWriteFileUntracked =
  Command 
    (\state ->
        Just $ do
          name <- Gen.string (Range.constant 1 10) Gen.alphaNum
          contents <- Gen.list (Range.constant 0 100) (Gen.string (Range.constant 0 100) Gen.ascii)
          pure $ CommandWriteFileUntracked (state_root state) (state_workDir state) name contents
    )
    (\(CommandWriteFileUntracked root workDir path content) -> do
      path' <- evalIO $ canonicalizePath (fromDir (root <> workDir) </> path)
      evalIO $ writeFile path' (unlines content)
    )
    [ Require $ \state (CommandWriteFileUntracked _ workDir name _) ->
        let dirs = Map.keys $ state_files state in
        (workDir `elem` dirs) &&
        (workDir <> toDir name) `notElem` dirs &&
        (fromDir workDir </> name) `notElem` state_trackedFiles state
    , Update $ \state (CommandWriteFileUntracked _ workDir name content) _output ->
        state{ state_files = Map.insertWith (<>) workDir (Map.singleton name $ File (unlines content) Untracked) (state_files state) }
    , Ensure $ \_pre _post _input _output -> do
        label "write file (untracked)"
        
        pure ()
    ]

data CommandWriteFileTracked (v :: Type -> Type) 
  = CommandWriteFileTracked
      -- | 'root'
      Dir
      -- | The path of the file, relative to 'root'.
      FilePath
      -- | The contents of the file; lines of text.
      [String]
  deriving (Show, Generic, FunctorB, TraversableB)

commandWriteFileTracked :: (MonadGen gen, MonadIO m, MonadTest m) => Command gen m State
commandWriteFileTracked =
  Command 
    (\state -> do
        let trackedFiles = state_trackedFiles state
        guard . not $ null trackedFiles
        Just $ do
          path <- Gen.element trackedFiles
          contents <- Gen.list (Range.constant 0 100) (Gen.string (Range.constant 0 100) Gen.ascii)
          pure $ CommandWriteFileTracked (state_root state) path contents
    )
    (\(CommandWriteFileTracked root path content) -> do
      path' <- evalIO . canonicalizePath $ fromDir root </> path
      evalIO $ writeFile path' (unlines content)
    )
    [ Require $ \state (CommandWriteFileTracked _ path _) ->
        path `elem` state_trackedFiles state
    , Update $ \state (CommandWriteFileTracked _ path content) _output ->
        let 
          (dir, name) = splitFileName path
          newContent = unlines content
        in
        state
          { state_files = 
              Map.adjust 
                (Map.adjust
                  (\(File _ status) -> File newContent status)
                  name
                )
                dir
                (state_files state)
          }
    , Ensure $ \_pre _post (CommandWriteFileTracked _ _path _content) _output -> do
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
      annotateShow toAddRelative
      (ec, stdout, stderr) <- evalIO $ runIn (root <> workDir) "git" ("add" : toAddRelative) ""
      annotateShow stdout
      annotateShow stderr
      evalExitCode ec
    )
    [ Require $ \state (CommandGitAdd _ workDir toAddAbsolute toAddRelative) ->
        let files = fst <$> state_allFiles state in
        workDir `Map.member` state_files state &&
        all (`elem` files) toAddAbsolute &&
        all (\file -> (fromDir workDir </> file) `elem` files) toAddRelative
    , Update $ \state (CommandGitAdd _ _workDir toAddAbsolute _toAddRelative) _output ->
        state
          { state_staged =
              Map.fromList
                (foldr 
                  (\file rest ->
                    let (dir, name) = splitFileName file in
                    case state_dir state dir Map.! name of
                      File newContent (Tracked oldContent) ->
                        if 
                          fromMaybe 
                            oldContent 
                            -- Check changes against the index when possible.
                            (Map.lookup file $ state_staged state)
                            /= newContent 
                        then (file, newContent) : rest
                        else rest
                      File newContent Untracked ->
                        (file, newContent) : rest
                  )
                  []
                  toAddAbsolute
                )
              `Map.union` 
              state_staged state 
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

        Map.keysSet (state_staged post) === stagedFiles
    ]

data CommandGitListUnstaged (v :: Type -> Type)
  = CommandGitListUnstaged
      -- | 'root'
      Dir
      -- | 'workDir'
      Dir
  deriving (Show, Generic, FunctorB, TraversableB)

state_unstaged :: State v -> Map FilePath File
state_unstaged state =
  Map.fromList 
    (filter
      (\case
        (path, File newContent (Tracked oldContent)) ->
          fromMaybe oldContent (Map.lookup path $ state_staged state) /= newContent
        (path, File newContent Untracked) ->
          case Map.lookup path $ state_staged state of
            Nothing -> False
            Just oldContent -> oldContent /= newContent
      ) $ 
      state_allFiles state
    )

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

        Map.keysSet (state_unstaged post) === unstagedFiles
    ]

data CommandGitListUntracked (v :: Type -> Type)
  = CommandGitListUntracked
      -- | 'root'
      Dir
  deriving (Show, Generic, FunctorB, TraversableB)

state_untracked :: State v -> [FilePath]
state_untracked state = do
  (dir, names) <- Map.toList $ state_files state
  (name, file) <- Map.toList names
  let path = fromDir dir </> name
  guard $ file_status file == Untracked && not (Map.member path (state_staged state))
  pure path

commandGitListUntracked :: (MonadGen gen, MonadIO m, MonadTest m) => Command gen m State
commandGitListUntracked =
  Command 
    (\state ->
      Just . pure $ CommandGitListUntracked (state_root state)
    )
    (\(CommandGitListUntracked root) -> do
      (ec, stdout, stderr) <- evalIO $ runIn root "git" ["ls-files", "--other", "--exclude-standard"] ""
      annotateShow stderr
      evalExitCode ec
    
      pure . Set.fromList $ ("." </>) <$> lines stdout
    )
    [ Ensure $ \_pre post _input untrackedFiles -> do
        label "git list untracked files"

        Set.fromList (state_untracked post) === untrackedFiles
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
        state
          { state_staged = Map.empty
          , state_files =
              Map.foldlWithKey'
                (\acc stagedFile stagedFileContent -> 
                  let (dir, name) = splitFileName stagedFile in
                  Map.adjust (Map.adjust (\(File newContent _) -> File newContent (Tracked stagedFileContent)) name) dir acc
                )
                (state_files state)
                (state_staged state)
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
        let unstaged = state_unstaged state in
        state
          { state_ustash = Just $ fmap file_contents unstaged 
          , state_files =
              Map.mapWithKey
                (\dir files ->
                  Map.mapWithKey
                    (\name file ->
                      let path = fromDir dir </> name in
                      if Map.member path unstaged
                      then 
                        case file_status file of
                          Tracked oldContent -> file{file_contents = fromMaybe oldContent $ Map.lookup path (state_staged state)}
                          _ -> undefined
                      else file
                    )
                    files
                )
                (state_files state)
          }
    , Ensure $ \_pre post _input () -> do
        label "git-ustash save"

        let unstaged = state_unstaged post
        annotateShow unstaged
        assert $ Map.null unstaged
    ]

data CommandGitUstashRestore (v :: Type -> Type) 
  = CommandGitUstashRestore
      -- | 'root'
      Dir
      -- | 'workDir'
      Dir
  deriving (Show, Generic, FunctorB, TraversableB)

commandGitUstashRestore :: (MonadGen gen, MonadIO m, MonadTest m) => Command gen m State
commandGitUstashRestore =
  Command 
    (\state ->
      Just . pure $ CommandGitUstashRestore (state_root state) (state_workDir state)
    )
    (\(CommandGitUstashRestore root workDir) -> do
      (ec, stdout, stderr) <- evalIO $ runIn (root <> workDir) "git-ustash" ["restore"] ""
      annotateShow stdout
      annotateShow stderr
      evalExitCode ec
    )
    [ Require $ \state (CommandGitUstashRestore _ workDir) -> 
        workDir `Map.member` state_files state &&
        state_hasCommits state &&
        isJust (state_ustash state)
    , Update $ \state (CommandGitUstashRestore _root _workDir) _output ->
        let ustashed = fromMaybe mempty $ state_ustash state in
        state
          { state_ustash = Nothing
          , state_files =
              Map.mapWithKey
                (\dir files ->
                  Map.mapWithKey
                    (\name file ->
                      let path = fromDir dir </> name in
                      case Map.lookup path ustashed of
                        Nothing ->
                          file
                        Just newContent ->
                          file{file_contents = newContent}
                    )
                    files
                )
                (state_files state)
          }
    , Ensure $ \pre post _input () -> do
        label "git-ustash restore"

        fmap file_contents (state_unstaged post) === 
          fromMaybe mempty (state_ustash pre) <> fmap file_contents (state_unstaged pre)
    ]
