{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}


instance (MonadIO m) => MondaIO (MaybeT m) where
  liftIO m = lift (liftIO m)


instance (MonadState s m) => MonadState s (MaybeT m) where
  get = lift get
  put k = lift (put k)
