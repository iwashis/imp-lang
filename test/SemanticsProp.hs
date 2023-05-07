module SemanticsProp where


import Semantics.BigStep as BS
import Semantics.SmallStep as SS     

import Language
import Semantics.Store 

prop_transitivity_store :: ( Eq a ) => ( Expr a, Store ) -> Bool
prop_transitivity_store (e, s) = 
    let maybeV  = BS.bigStep (e, s)
        maybeV' = do 
            (e',s') <- SS.step (e, s) 
            BS.bigStep (e',s')
    in maybeV == maybeV'
