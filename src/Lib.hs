{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( runBot
    ) where

import           Data.Text                      ( append )
import qualified Data.Text.IO                  as TIO
import qualified Data.Text                     as T
import           Control.Lens                   ( (.~)
                                                , (<&>)
                                                , (^.)
                                                , (&)
                                                )
import qualified Discord                       as D
import qualified Discord.Types                 as D
import qualified Discord.Requests              as R
import           Data.IORef                     ( newIORef
                                                , readIORef
                                                , writeIORef
                                                , IORef
                                                )
import           Control.Concurrent             ( forkIO )
import           Control.Monad                  ( void
                                                , when
                                                , mapM
                                                )
import           System.Environment             ( lookupEnv )
import qualified Control.Monad.Reader          as Reader
import           Control.Monad.Trans            ( liftIO )
import           Network.Google.AppsCalendar    ( AppsCalendarAPI )
import qualified Servant
import qualified Servant                       as Servant
import qualified Servant.Client                as Servant
import qualified Network.HTTP.Client           as HTTP
import qualified System.Environment            as Environment
import           Control.Monad.Trans.Resource   ( runResourceT )
import qualified Network.Google                as Google
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base64.URL    as Base64URL
import qualified Network.Google.AppsCalendar   as Calendar
import           Network.HTTP.Client            ( newManager )
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           System.IO                      ( stdout )
import qualified Data.Maybe                    as Maybe
import qualified Data.Time                     as Time
import qualified Data.Time.Format              as Time

runBot :: IO ()
runBot = do
  tokenM <- lookupEnv "EVENTS_DISCORD_TOKEN"
  case tokenM of
    Nothing -> 
      putStrLn "Expected environment variable EVENTS_DISCORD_TOKEN to log in to Discord"
    Just token -> do
      userFacingError <- D.runDiscord $ D.def
        { D.discordToken   = T.pack token
        , D.discordOnEvent = eventHandler
        }
      TIO.putStrLn userFacingError

eventHandler :: D.Event -> D.DiscordHandler ()
eventHandler event =  Reader.ask >>= \_ ->
  case event of
  D.MessageCreate m -> 
      when (not $ D.userIsBot (D.messageAuthor m) ) $
        case (D.messageText m) of
          "!help" -> do
            void $ D.restCall (R.CreateMessage (D.messageChannel m) helpMessage)
          "!events" -> do
            events <- liftIO printNewEvents
            sendEmbed (D.messageChannel m) events
          _ -> pure ()

        
  _ -> pure ()
 where
  printMessage :: D.Message -> String
  printMessage message = T.unpack $ D.userName (D.messageAuthor message) <> ": " <> D.messageText message

showEventText :: Calendar.Event -> IO T.Text
showEventText event = 
  let startTime = (event^.(Calendar.eStart) >>= (^. Calendar.edtDateTime))
  in
  case startTime of
    Just start -> do
      zone <- Time.getTimeZone start
      let zonedTime = Time.utcToZonedTime zone start
          name = Maybe.fromMaybe "" $  event^.Calendar.eSummary
          time = Time.formatTime Time.defaultTimeLocale "%a %d/%m %R" zonedTime
       in
        return $ T.unwords [ "-", name, T.pack time]
    Nothing ->
      return $ ""

sendEmbed :: D.ChannelId -> Calendar.Events -> D.DiscordHandler ()
sendEmbed channel events = do
    eventNames <- liftIO $ mapM showEventText (events^.Calendar.eveItems)
    let embed = D.CreateEmbed 
                { D.createEmbedAuthorName = ""
                , D.createEmbedAuthorUrl = ""
                , D.createEmbedAuthorIcon = Nothing
                , D.createEmbedTitle = "Programming Club Events"
                , D.createEmbedUrl = ""
                , D.createEmbedThumbnail = Nothing
                , D.createEmbedDescription = T.unlines eventNames
                , D.createEmbedFields = []
                , D.createEmbedImage = Nothing
                , D.createEmbedFooterText = ""
                , D.createEmbedFooterIcon = Nothing
                }
    void $ D.restCall (R.CreateMessageEmbed channel "Here are our upcoming events:" embed)

printNewEvents :: IO Calendar.Events
printNewEvents = do 
  let email = Base64URL.encode $ "samnolan555@gmail.com"
  putStrLn "Getting new events"
  lgr <- Google.newLogger Google.Debug stdout
  putStrLn "Getting manager"
  mgr <- newManager tlsManagerSettings
  putStrLn "Getting credentials"
  crd <- Google.getApplicationDefault mgr
  putStrLn "Getting env"
  env <-
    Google.newEnvWith crd lgr mgr <&> (Google.envScopes .~ Calendar.calendarScope)
  putStrLn "Send request"
  now <- Time.getCurrentTime
  r <-
    runResourceT . Google.runGoogle env . Google.send $
    (Calendar.eventsList "cp40h0ol4t449m0tq0nmhtjnss@group.calendar.google.com" & Calendar.elTimeMin .~ (Just now))
  return $ r

helpMessage :: T.Text
helpMessage = "Hello! I'm an events bot. You can see upcoming events with !events"
