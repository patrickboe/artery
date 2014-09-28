import GHC.RTS.Events

analyze path analysis =
  do logread <- readEventLogFromFile path
     print (do log <- logread
               return $ analysis log)

isTrace (Event t i) = case i of
                        (UserMessage x) -> True
                        _ -> False

dfilter f = foldr ((++) . rfilter) []
  where rfilter e@(Event _ i) =
          case i of
            (EventBlock _ _ es) -> dfilter f es
            _ | f e -> [e]
            otherwise -> []


listTraces = (dfilter isTrace) . events . dat

matchingMsg m = (filter mtch) . listTraces
  where
    mtch (Event t ei) = msg ei == m
