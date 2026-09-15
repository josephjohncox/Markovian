{-# LANGUAGE DataKinds #-}

module GraphGradientUpdate where

import Markovian.Backend.GPU.Graph

badGradientUpdate :: GraphGradient -> GraphGradient
badGradientUpdate value = value{graphGradientValues = [0]}
