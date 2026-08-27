{-# LANGUAGE DataKinds #-}

module PushoutKind where

import Markovian.Open.Pushout (FinitePushout, PushoutPoint, pushoutClasses)

breakPushoutKind :: FinitePushout Bool 'True Int Int -> [PushoutPoint Int Int]
breakPushoutKind = pushoutClasses
