{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Strict #-}

module PathStats
  ( PathStats (..),
    calculatePathStats,
    module StorePath,
  )
where

import StorePath

data PathStats
  = PathStats
      { psTotalSize :: Int,
        psAddedSize :: Int
      }
  deriving (Show)

calculatePathStats :: StoreEnv () -> StoreEnv PathStats
calculatePathStats env =
  let envWithTotals =
        transformStoreEnv
          ( \sp@StorePath {spName} ->
              let refs = fetchAllReferences env (const True) spName
               in PathStats
                    { psTotalSize = sum (map spSize refs) + spSize sp,
                      psAddedSize = 0
                    }
          )
          env
      totalSize =
        psTotalSize . spPayload $
          unsafeLookupStoreEnv envWithTotals (seTop env)
   in transformStoreEnv
        ( \StorePath {spName, spPayload} ->
            let otherRefs = fetchAllReferences env (/= spName) (seTop env)
                otherSize = sum (map spSize otherRefs)
                addedSize = totalSize - otherSize
             in spPayload
                  { psAddedSize = addedSize
                  }
        )
        envWithTotals
