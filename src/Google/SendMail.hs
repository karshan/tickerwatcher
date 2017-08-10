{-# LANGUAGE OverloadedStrings #-}
module Google.SendMail where

import Control.Lens
import Control.Monad
import Network.Google ((!))
import qualified Network.Google as Google
import qualified Network.Google.Auth as Google
import qualified Network.Google.Gmail as Google
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Mail.Mime
import System.IO (stdout)

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Base64.URL as B64Url
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Monoid

sendMail :: FilePath -> Text -> Text -> Text -> TL.Text -> IO Google.Message
sendMail svcAccKey svcAccUser to subject msg = do
    rawMail <- B64Url.encode . toStrict <$> renderMail' (simpleMail' (Address Nothing to) (Address Nothing svcAccUser) subject msg)
    mgr <- newManager tlsManagerSettings
    creds <- Google.serviceAccountUser (Just svcAccUser) <$> Google.fromFilePath svcAccKey
    env <- (Google.envScopes .~ Google.gmailSendScope) <$> Google.newEnvWith creds (\_ _ -> pure ()) mgr
    Google.runResourceT . Google.runGoogle env $
        Google.send (Google.usersMessagesSend (Google.message & Google.mRaw .~ Just rawMail) & Google.umsUserId .~ svcAccUser)
