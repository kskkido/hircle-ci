module Fixtures.Modules.Github.Models.Commit.Main
  ( test1
  , test2
  ) where

import qualified Modules.Github.Models.Commit.Main as Commit

test1 :: Commit.Commit
test1 = Commit.Commit
  { Commit.repository = "repository"
  , Commit.sha        = "sha"
  , Commit.branch     = "branch"
  , Commit.author     = "author"
  , Commit.message    = "message"
  }

test2 :: Commit.Commit
test2 = Commit.Commit
  { Commit.repository = "hircle-ci/hircle"
  , Commit.sha        = "00000"
  , Commit.branch     = "master"
  , Commit.author     = "hircle-ci"
  , Commit.message    = "test commit"
  }
