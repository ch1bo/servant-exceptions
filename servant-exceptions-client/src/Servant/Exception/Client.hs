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
import Servant.API.ContentTypes                   (AllMimeRender, allMime, allMimeRender)
import Servant.Exception                          (ToServantErr(..), Throws)
import GHC.TypeLits (Nat)

import Servant.API                                hiding (Header)
import Data.Proxy
import Servant.Client.Core

-- * Type level annotated exception handling

instance ( Exception e
         , ToServantErr e
         , AllMimeRender ct e
         , RunClient m
         , HasClient m (Verb mt st ct (Either e a))
         ) => HasClient m (Throws e :> Verb (mt :: k) (st :: Nat) (ct :: [*]) (a :: *)) where

  type Client m (Throws e :> Verb mt st ct a) = Client m (Verb mt st ct (Either e a))

  clientWithRoute pm Proxy req = clientWithRoute pm (Proxy :: Proxy (Verb mt st ct (Either e a))) req

  hoistClientMonad pm Proxy f cm = hoistClientMonad pm (Proxy :: Proxy (Verb mt st ct (Either e a))) f cm

-- | Push @Throws@ further "upstream".
instance ( RunClient m
         , HasClient m next
         , next ~ (api :> Throws e :> upstream)
         ) => HasClient m (Throws e :> api :> upstream) where

  type Client m (Throws e :> api :> upstream) = Client m (api :> Throws e :> upstream)

  clientWithRoute pm Proxy req = clientWithRoute pm (Proxy :: Proxy next) req

  hoistClientMonad pm Proxy f cm = hoistClientMonad pm (Proxy :: Proxy next) f cm


-- | Transitive application of @Throws@ on @(:<|>)@.
instance ( RunClient m
         , HasClient m next
         , next ~ (Throws e :> api1 :<|> Throws e :> api2)
         ) => HasClient m (Throws e :> (api1 :<|> api2)) where

  type Client m (Throws e :> (api1 :<|> api2)) = Client m (Throws e :> api1 :<|> Throws e :> api2)

  clientWithRoute pm Proxy req = clientWithRoute pm (Proxy :: Proxy next) req

  hoistClientMonad pm Proxy f cm = hoistClientMonad pm (Proxy :: Proxy next) f cm
