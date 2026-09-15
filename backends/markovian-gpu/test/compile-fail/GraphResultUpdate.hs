{-# LANGUAGE DataKinds #-}

module GraphResultUpdate where

import Markovian.Backend.GPU.Graph

badResultUpdate :: GraphResult 2 2 -> GraphResult 2 2
badResultUpdate value = value{graphResultValues = [0]}
