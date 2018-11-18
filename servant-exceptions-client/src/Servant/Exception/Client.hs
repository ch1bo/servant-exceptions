{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Exception.Client where

import Control.Exception (Exception)
-- import Servant.API.ContentTypes                   (AllMimeRender, allMime, allMimeRender)
import Servant.Exception                          (ToServantErr(..), Throws)

import Servant.API                                hiding (Header)
import Data.Proxy
import Servant.Client.Core

-- * Type level annotated exception handling

instance ( Exception e
         , ToServantErr e
         -- , AllMimeRender ct e
         , RunClient m
         , HasClient m api
         --, HasClient (Verb mt st ct a) context
         -- ) => HasClient m (Throws e :> Verb (mt :: k) (st :: Nat) (ct :: [*]) (a :: *)) where
         ) => HasClient m (Throws e :> api) where

  type Client m (Throws e :> api) = Client m api

  clientWithRoute pm Proxy req = clientWithRoute pm (Proxy :: Proxy api) req

  hoistClientMonad pm Proxy f cm = hoistClientMonad pm (Proxy :: Proxy api) f cm

{-
 --- | Push @Throws@ further "upstream".
 -instance HasClient (api :> Throws e :> upstream) context =>
 -         HasClient (Throws e :> api :> upstream) context where
 -
 --- | Transitive application of @Throws@ on @(:<|>)@.
 -instance HasClient (Throws e :> api1 :<|> Throws e :> api2) context =>
 -         HasClient (Throws e :> (api1 :<|> api2)) context where
 -}
