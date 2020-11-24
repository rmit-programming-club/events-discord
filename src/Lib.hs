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
                                                , forM
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
import qualified Data.Time.Calendar            as Time
import qualified Data.Time.Format              as Time
import qualified Data.List                     as List
import qualified Data.Time.RRule               as RRule
import qualified Data.List.NonEmpty            as NonEmpty

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
            events <- liftIO fetchNewEvents
            sendEmbed (D.messageChannel m) events
          _ -> pure ()

        
  _ -> pure ()
 where
  printMessage :: D.Message -> String
  printMessage message = T.unpack $ D.userName (D.messageAuthor message) <> ": " <> D.messageText message

showEventText :: ClubEvent -> IO T.Text
showEventText event = do
  let startTime = clubEventStart event
      name = clubEventName event
  return $ T.unwords [ "-", T.pack (show startTime), name, "by", (clubEventClub event)]


eventUTCTime :: ClubEventTime ->  Time.UTCTime
eventUTCTime time = 
    case time of
      ClubEventTimeDate date zone ->
        Time.zonedTimeToUTC (Time.ZonedTime (Time.LocalTime date Time.midnight) zone)
      ClubEventTimeDateTime zonedTime ->
        Time.zonedTimeToUTC zonedTime
        
  


sendEmbed :: D.ChannelId -> [ClubEvent] -> D.DiscordHandler ()
sendEmbed channel events = do
    zone <- liftIO Time.getCurrentTimeZone
    eventNames <- liftIO $ mapM showEventText (List.sortOn (eventUTCTime . clubEventStart) events)
    let embed = D.CreateEmbed 
                { D.createEmbedAuthorName = ""
                , D.createEmbedAuthorUrl = ""
                , D.createEmbedAuthorIcon = Nothing
                , D.createEmbedTitle = "Upcoming Club Events"
                , D.createEmbedUrl = ""
                , D.createEmbedThumbnail = Nothing
                , D.createEmbedDescription = T.unlines eventNames
                , D.createEmbedFields = []
                , D.createEmbedImage = Nothing
                , D.createEmbedFooterText = ""
                , D.createEmbedFooterIcon = Nothing
                }
    void $ D.restCall (R.CreateMessageEmbed channel "Here are our upcoming events:" embed)

data SavedCalendar = SavedCalendar 
                     { savedCalandarTitle :: T.Text
                     , savedCalandarId :: T.Text
                     }

savedCalendars :: [SavedCalendar]
savedCalendars = 
  [ SavedCalendar "The Programming Club" "cp40h0ol4t449m0tq0nmhtjnss@group.calendar.google.com"
  , SavedCalendar "CSIT Society" "723mf4l2iplkucoatmgi2ps8fs@group.calendar.google.com"
  , SavedCalendar "RISC" "rmitinfosecollective@gmail.com"
  ]
 
data ClubEvent = ClubEvent
                 { clubEventClub :: T.Text
                 , clubEventName :: T.Text
                 , clubEventStart :: ClubEventTime
                 }
              deriving (Show)
data ClubEventTime = ClubEventTimeDateTime Time.ZonedTime 
                  | ClubEventTimeDate Time.Day Time.TimeZone

instance Show ClubEventTime where
  show (ClubEventTimeDateTime time) = Time.formatTime Time.defaultTimeLocale "%a %d/%m %R" time
  show (ClubEventTimeDate date zone) = Time.formatTime Time.defaultTimeLocale "%a %d/%m" date

fetchNewEvents :: IO [ClubEvent]
fetchNewEvents = do 
  let email = Base64URL.encode $ "samnolan555@gmail.com"
  lgr <- Google.newLogger Google.Debug stdout
  mgr <- newManager tlsManagerSettings
  crd <- Google.getApplicationDefault mgr
  env <-
    Google.newEnvWith crd lgr mgr <&> (Google.envScopes .~ Calendar.calendarScope)
  now <- Time.getCurrentTime
  events <- concat <$> forM savedCalendars (\(SavedCalendar clubName calendarId) -> do
    r <-
      runResourceT . Google.runGoogle env . Google.send $
      (Calendar.eventsList calendarId & Calendar.elTimeMin .~ (Just now))
    concat <$> mapM  (expandEvent clubName now  (Time.addUTCTime (Time.nominalDay * 21) now)) (r^.Calendar.eveItems)
    )
  return events

toClubEventTime :: Calendar.EventDateTime -> IO ClubEventTime 
toClubEventTime dateTime = 
  case dateTime ^. Calendar.edtDateTime of
    Just time ->
        ClubEventTimeDateTime .  flip Time.utcToZonedTime time <$> Time.getTimeZone time
    Nothing -> 
      case dateTime ^. Calendar.edtDate of
        Just date ->
           ClubEventTimeDate date <$> Time.getCurrentTimeZone 
        Nothing ->
          fail "Could not parse date"
          
        
rruleDayToDayOfWeek :: RRule.Day -> Time.DayOfWeek
rruleDayToDayOfWeek RRule.Monday = Time.Monday
rruleDayToDayOfWeek RRule.Tuesday = Time.Tuesday
rruleDayToDayOfWeek RRule.Wednesday = Time.Wednesday
rruleDayToDayOfWeek RRule.Thursday = Time.Thursday
rruleDayToDayOfWeek RRule.Friday = Time.Friday
rruleDayToDayOfWeek RRule.Saturday = Time.Saturday
rruleDayToDayOfWeek RRule.Sunday = Time.Sunday

dayOfWeekDiff :: Time.DayOfWeek -> Time.DayOfWeek -> Int
dayOfWeekDiff a b = mod (fromEnum a - fromEnum b) 7

-- | The first day-of-week on or after some day
firstDayOfWeekOnAfter :: Time.DayOfWeek -> Time.Day -> Time.Day
firstDayOfWeekOnAfter dw d = Time.addDays (toInteger $ dayOfWeekDiff dw $ Time.dayOfWeek d) d

expandEvent :: T.Text -> Time.UTCTime -> Time.UTCTime -> Calendar.Event -> IO [ClubEvent]
expandEvent clubName start end event = do
  let recurrence =  event ^. Calendar.eRecurrence
  case event ^. Calendar.eStart of
    Just st -> do
      startTime <- toClubEventTime st
      zone <- Time.getTimeZone start
      return $ Maybe.fromMaybe [] $ do
        title <- event ^. Calendar.eSummary
        case event ^. Calendar.eRecurrence of
          [] ->
            return $ [ClubEvent clubName title startTime]
          recurrence : _ -> do
            rule <- RRule.fromText recurrence
            byDay <- RRule.byDay rule
            frequency <- RRule.frequency rule
            let (Time.ZonedTime (Time.LocalTime startDate _) _) = Time.utcToZonedTime zone start
            return  . concat $ map (\(_, day) -> 
              let dayOfWeek = rruleDayToDayOfWeek day
                  startingWeekDate = firstDayOfWeekOnAfter dayOfWeek startDate
              in
                case frequency of
                  RRule.Weekly -> 
                    case startTime of
                      ClubEventTimeDate date zone -> 
                        let possibleDays = map (flip ClubEventTimeDate zone . flip Time.addDays startingWeekDate) [0,7..]
                            boundedDays = takeWhile ((< end) . eventUTCTime) possibleDays
                        in
                          map (\time -> ClubEvent clubName title time) boundedDays
                      ClubEventTimeDateTime zonedTime -> 
                        let (Time.ZonedTime (Time.LocalTime date timeOfDay) zone) = zonedTime
                            possibleTimes = map ((\posDate -> (ClubEventTimeDateTime (Time.ZonedTime (Time.LocalTime posDate timeOfDay) zone) )). flip Time.addDays startingWeekDate) [0,7..]
                            boundedDays = takeWhile ((< end) . eventUTCTime) possibleTimes
                        in
                          map (\time -> ClubEvent clubName title time) boundedDays

                  _ -> 
                    return $ ClubEvent clubName title startTime
                ) (NonEmpty.toList byDay)

    Nothing ->
      return []
  

helpMessage :: T.Text
helpMessage = "Hello! I'm an events bot. You can see upcoming events with !events"
