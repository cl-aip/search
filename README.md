# search
PAIP original programs on search is modernized here.

The aim of search.lisp is to teach some of search technique in AI, Depth First Search, Breadth First Search, Best First Search, and A* (pronounced [eI] star) algorithm. <br />
The example adopted here is finding a flight route from Boston to San Francisco by a small airplane that can fly only 1000 km on one flight. This example is roughly same as PAIP, but
the exact lat&long data of actual airport location was prepared rather than city data as follows.
```
(defparameter *cities*
  '((Atlanta      ATL 33.636667  -84.428056)
    (Los-Angeles   LAX 33.9425   -118.408056)
    (Boston       BOS 42.363056  -71.006389)
    (Memphis       MEM 35.0425    -89.976667)  
    (Chicago      ORD 41.978611  -87.904722)
    (New-York      LGA 40.77725   -73.872611) 
    (Denver       DEN 39.861656 -104.673178)
    (Oklahoma-City OKC 35.393056  -97.600833)
    (Eugene       EUG 44.123056 -123.218611)
    (Pittsburgh    PIT 40.491389  -80.232778) 
    (Flagstaff    FLG 35.140278 -111.669167)
    (Quebec        YQB 46.791111  -71.393333)
    (Grand-Jct    GJT 39.1225   -108.526667)
    (Reno          RNO 39.499167 -119.768056)
    (Houston      IAH 29.984444  -95.341389)
    (San-Francisco SFO 37.618889 -122.375)
    (Indianapolis IND 39.717222  -86.294444)
    (Tampa         TPA 27.975556  -82.533333)
    (Jacksonville JAX 30.494167  -81.687778)
    (Victoria      YYJ 48.647222 -123.425833)
    (Kansas-City  MCI 39.2975    -94.713889)
    (Wilmington    ILM 34.270556  -77.9025)))
