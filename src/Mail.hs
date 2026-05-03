module Mail where

import           Data.Text    (Text)

import           Type.General (Email (..))

data MailService = SMTP | Mock

sendVerificationMail :: MailService -> Email -> Text -> IO Bool
sendVerificationMail Mock (Email email) token = do
  putStrLn $ line
  putStrLn $ "Email : " ++ (show email)
  putStrLn $ "Token : " ++ (show token)
  putStrLn $ line
  pure True
    where line = replicate 10 '='

sendVerificationMail SMTP email token = undefined
