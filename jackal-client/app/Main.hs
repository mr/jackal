{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Main where
--
import Reactive.Banana
import Reactive.Banana.GI.Gtk
import Reactive.Banana.Frameworks

import Data.Monoid ((<>))
import Data.Maybe

import Control.Concurrent
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Control.Concurrent.Async
import Data.IORef
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Network.HTTP.Conduit
import Control.Monad
import Network.RTorrent hiding (start)

import Control.Exception (catch)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Functor (($>))

import Data.Maybe (fromJust)
import qualified GI.Gtk as Gtk
import Data.GI.Base.ManagedPtr
    ( castTo
    )
import Data.GI.Base.GObject
    ( gtypeFromInstance
    )
import GI.Gtk
    ( mainQuit
    , GObject(..)
    , ManagedPtr(..)
    , builderAddFromFile
    , builderNew
    , Window(..)
    , HeaderBar(..)
    , Stack(..)
    , ListBox(..)
    , Button(..)
    , Label(..)
    , GError(..)
    , get
    , set
    , on
    , gerrorMessage
    , gtypeName
    , SignalProxy(..)
    , AttrOp(..)
    , new
    , ListBoxRow(..)
    , IsWidget(..)
    , toWidget
    , Widget(..)
    , FileChooserDialog(..)
    , ResponseType(..)
    )

import GI.Gdk
    ( threadsAddIdle
    )

import qualified GI.GLib.Constants as GLC
import qualified Data.ByteString as BS
import Servant.Client

import Network.Jackal.Client

data StackPage = Search | Downloads

instance Show StackPage where
    show Search = "search"
    show Downloads = "downloads"

showT :: Show a => a -> Text
showT = T.pack . show

threadsAddOnce :: IO a -> IO ()
threadsAddOnce m = void $ threadsAddIdle GLC.PRIORITY_DEFAULT_IDLE (m $> False)

(#>) :: Event a -> IO b -> MomentIO ()
(#>) e a = reactimate $ e $> void a

(<#) :: IO b -> Event a -> MomentIO ()
(<#) = flip (#>)

(<#>) :: Event a -> (a -> IO b) -> MomentIO ()
(<#>) e f = reactimate $ (void . f) <$> e

(##>) :: Event a -> IO b -> MomentIO ()
(##>) e a = e #> threadsAddOnce a

(<##) :: IO b -> Event a -> MomentIO ()
(<##) = flip (##>)

(<##>) :: Event a -> (a -> IO b) -> MomentIO ()
(<##>) e f = e <#> threadsAddOnce . f

infixr 1 #>
infixr 1 <#>
infixr 1 ##>
infixr 1 <##>

filterMapM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
filterMapM _ [] = return []
filterMapM f (x:xs) = do
    my <- f x
    case my of
        Just y -> (y :) <$> filterMapM f xs
        Nothing -> filterMapM f xs

castAllTo
    :: (GObject o, GObject o')
    => (ManagedPtr o' -> o')
    -> [o]
    -> IO [o']
castAllTo = filterMapM . castTo

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

data ListAdapter s m = ListAdapter {
    createCell :: m Widget,
    updateCell :: Widget -> s -> m ()
}

updateCells
    :: ListBox
    -> ListAdapter s IO
    -> [ListBoxRow]
    -> [s]
    -> IO ()
updateCells _ _ [] [] = return ()
updateCells listBox adapter [] (s:ss) = do
    w <- createCell adapter
    #add listBox w
    updateCell adapter w s
    updateCells listBox adapter [] ss
updateCells listBox adapter (lbr:lbrs) [] = do
    #remove listBox lbr
    updateCells listBox adapter lbrs []
updateCells listBox adapter (lbr:lbrs) (s:ss) = do
    w <- head <$> #getChildren lbr
    updateCell adapter w s
    updateCells listBox adapter lbrs ss

useAdapter
    :: ListBox
    -> ListAdapter s IO
    -> [s]
    -> IO ()
useAdapter listBox adapter ss = do
    listBoxRowsW <- #getChildren listBox
    listBoxRows <- castAllTo ListBoxRow listBoxRowsW
    updateCells listBox adapter listBoxRows ss
    #showAll listBox

driveListBox
    :: Behavior [s]
    -> ListBox
    -> ListAdapter s IO
    -> MomentIO ()
driveListBox b listBox adapter = do
    let adapt = threadsAddOnce . useAdapter listBox adapter
    valueBLater b >>= liftIOLater . adapt
    sse <- changes b
    reactimate' $ fmap adapt <$> sse

printGType :: (GObject o) => o -> IO ()
printGType = print <=< gtypeName <=< gtypeFromInstance

goodTail :: [a] -> [a]
goodTail (_:xs) = xs
goodTail [] = []

callServer :: Manager -> ClientM a -> IO ()
callServer m c = void $ runClientM c (mkClientEnv m (BaseUrl Http "localhost" 8080 ""))

networkDescription :: MomentIO ()
networkDescription = do
    -- kick off download thread
    m <- liftIO $ newManager tlsManagerSettings

    b <- builderNew
    builderAddFromFile b "test.ui"

    window <- castB b "window" Window
    destroyE <- signalE0 window #destroy
    destroyE #> mainQuit

    -- start torrent when button pressed
    button <- castB b "button" Button
    pressedE <- signalE0 button #clicked
    pressedE #> do
        dialog <- new FileChooserDialog [#transientFor := window]
        #addButton dialog "Select Torrent"
            (fromIntegral $ fromEnum ResponseTypeAccept)
        #addButton dialog "Cancel" (fromIntegral $ fromEnum ResponseTypeCancel)
        response <- toEnum . fromIntegral <$> #run dialog
        case response of
            ResponseTypeAccept -> do
                filenames <- #getFilenames dialog
                void $ async $ forM filenames $ \filename -> do
                    torrent <- BS.readFile filename
                    callServer m (start torrent)
            _ -> return ()
        #hide dialog

    listBox <- castB b "listBox" ListBox
    refresh <- castB b "refresh" Button
    refreshedE <- signalE0 refresh #clicked

    --driveListBox stateB listBox $ ListAdapter {
    --    createCell = new Label [] >>= toWidget,
    --    updateCell = \w s -> do
    --        label <- fromJust <$> castTo Label w
    --        label `set` [#label := s]
    --}

    #showAll window

runGtk :: IO ()
runGtk = do
    Gtk.init Nothing
    compile networkDescription >>= actuate
    Gtk.main

main :: IO ()
main = runGtk `catch` (\(e::GError) -> gerrorMessage e >>= putStrLn . T.unpack)
