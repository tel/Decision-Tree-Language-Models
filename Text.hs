{-# LANGUAGE ForeignFunctionInterface #-}
module Text where

import Stats (Freq, freqFrom)

-- Get the sliding window bigrams of a bytestring
bigrams :: String -> [(Char, Char)]
bigrams str = zip str (tail str)
        

-- Some textual statistics
unigram_f :: String -> Freq Char        
unigram_f = freqFrom     

bigram_f :: String -> Freq (Char, Char)
bigram_f = freqFrom . bigrams