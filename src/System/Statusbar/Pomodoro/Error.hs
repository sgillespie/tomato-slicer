module System.Statusbar.Pomodoro.Error
  ( ServerError (..),
    ClientError (..),
  ) where

data ServerError
  = JsonParseError Text
  | ServerUnexpectedError
  deriving stock (Eq, Ord, Show)

instance Exception ServerError

data ClientError
  = ClientUnexpectedError
  deriving stock (Eq, Ord, Show)

instance Exception ClientError
