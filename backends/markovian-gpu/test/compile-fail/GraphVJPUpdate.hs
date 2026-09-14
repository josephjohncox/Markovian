{-# LANGUAGE DataKinds #-}

module GraphVJPUpdate where

import Markovian.Backend.GPU.Graph

badVJPUpdate :: GraphVJP 2 2 -> GraphVJP 2 2
badVJPUpdate value = value{graphVJPGradients = []}
