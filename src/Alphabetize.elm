module Alphabetize exposing
    ( Options, SortMode(..), NumberSort(..)
    , alphabetical
    , indexSort
    )

{-| Alphabetize is a library for sorting English strings the way editors and writers tend to sort them, not how computers do.

##A Note About Proper Nouns
Many proper nouns, such as people and place names, are very complicated to sort.

-Franklin Delano Roosevelt would be sorted under R, for Roosevelt (Delano being his middle name), but Andrew Lloyd Webber would be sorted under L (as his is an unhyphenated double-barreled surname).
-Korean film director Park Chan-Wook ought to be sorted under P, as Park is his family name.
-Lake Geneva, WI would be sorted under L, but the lake itself would be sorted under G.

For these reasons, no attempt is made to identify proper nouns or distingish between different categories of them.

Likewise, no attempt is made to alter the strings being sorted. A person's name is almost always formatted in an index as "Last, First Middle"; however, Alphabetical will need to have strings provided in this format to be sorted accordingly.


# Definition

@docs Options, SortMode, NumberSort


# Comparitor

@docs alphabetical

-}

import Regex
import String


{-| Letter-by-letter sorts strings as though every string were a single word; word-by-word sort sorts multiple-word phrases together by initial words. Thus, in letter-by-letter sort, "New Zealand" comes after "Newton", becauze Z comes after T; in word-by-word sort, "New Zealand" comes before "Newton" because all phrases beginning with the word "new" come before words beginning the with the letters n-e-w.
-}
type SortMode
    = LetterByLetter
    | WordByWord


{-| Whether to sort numbers according to their numerical value or the spelling of the English word.
-}
type NumberSort
    = Alphabetically
    | Numerically


{-|

  - `sortMode` lets you choose between sorting letter by letter or word by word
  - `initialNumberSort` sets a `NumberSort` for numbers encountered at the beginning of a string.
  - `internalNumberSort` sets a `NumberSort` for numbers encountered after at least some letters
  - When `years` is `True`, Alphabetical will attempt to sort four-digit numbers in English words the way they are normally pronounced as years, e.g. "1984" becomes "Nineteen Eighy Four" instead of "One Thousand, Nine Hundred Eighty Four"
  - When `romanNumberals` is `True`, it will attempt to sort strings that resemble Roman Numerals as decimal numbers, sorted according to `initialNumberSort` or `internalNumberSort` as appropriate
  - when `ignoreInitialArticle` is true, Alphabetical sorts phrases that begin with English articles "The" or "A" according to whatever comes after the article.

-}
type alias Options =
    { sortMode : SortMode
    , initialNumberSort : NumberSort
    , internalNumberSort : NumberSort
    , years : Bool
    , romanNumerals : Bool
    , ignoreInitialArticle : Bool
    }


{-| `indexSort` is a set of options that corresponds to how the index of a standard English-language non-fiction book would be sorted, and is probably the sort you want if you are sorting a list of English proper nouns, like movie titles or band names.
-}
indexSort =
    { sortMode = WordByWord
    , initialNumberSort = Alphabetically
    , internalNumberSort = Numerically
    , years = False
    , romanNumerals = False
    , ignoreInitialArticle = True
    }


{-| `naturalSort` is a set of options that corresponds to the concept of "naturally" sorting strings like filenames, for instance [as described as at Coding Horror ](https://blog.codinghorror.com/sorting-for-humans-natural-sort-order/).
-}
naturalSort =
    { sortMode = LetterByLetter
    , initialNumberSort = Numerically
    , internalNumberSort = Numerically
    , years = False
    , romanNumerals = False
    , ignoreInitialArticle = False
    }


processAll : String -> String
processAll string =
    let
        validCharacters =
            "[^A-Za-z0-9À-ÿ() ]"

        validCharacterRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString validCharacters
    in
    string
        -- All strings need to be lowercased for proper comparison
        -- No known alphabetizing standards care about case
        |> String.toLower
        -- Treat parens as whitespace
        |> String.replace "(" " "
        |> String.replace ")" " "
        |> Regex.replace validCharacterRegex (\_ -> "")



-- And prune all other non-alphanumeric characters


processForLetterByLetter : String -> String
processForLetterByLetter =
    String.replace " " ""


processForWordByWord : String -> String
processForWordByWord =
    String.replace " " "A"


processForSortMode : SortMode -> String -> String
processForSortMode sortMode =
    case sortMode of
        LetterByLetter ->
            processForLetterByLetter

        WordByWord ->
            processForWordByWord


processForInitialNumberSort : NumberSort -> String -> String
processForInitialNumberSort numberSort =
    identity


processForInternalNumberSort : NumberSort -> String -> String
processForInternalNumberSort numberSort =
    identity


processForYears : Bool -> String -> String
processForYears years =
    identity


processForRomanNumerals : Bool -> String -> String
processForRomanNumerals romanNumerals =
    identity


startsWithThe : String -> Bool
startsWithThe string =
    String.startsWith "the " string


startsWithA : String -> Bool
startsWithA string =
    String.startsWith "a " string


processForInitialArticles : Bool -> String -> String
processForInitialArticles ignoreInitialArticles string =
    case ignoreInitialArticles of
        True ->
            if startsWithThe string then
                String.dropLeft 4 string

            else if startsWithA string then
                String.dropLeft 2 string

            else
                string

        False ->
            string


process : Options -> String -> String
process { sortMode, initialNumberSort, internalNumberSort, years, romanNumerals, ignoreInitialArticle } string =
    string
        -- Run all neutral processing steps
        |> processAll
        -- First, perform whitespace-dependent processing!
        |> processForInitialArticles ignoreInitialArticle
        |> processForYears years
        -- and perform whitespace-independent processing
        |> processForInitialNumberSort initialNumberSort
        |> processForInternalNumberSort internalNumberSort
        |> processForRomanNumerals romanNumerals
        -- Now, handle whitespace appropriately
        |> processForSortMode sortMode


alphabetical : Options -> String -> String -> Order
alphabetical options stringA stringB =
    let
        processedA =
            Debug.log "Processed A: " (process options stringA)

        processedB =
            Debug.log "Processed B: " (process options stringB)
    in
    compare processedA processedB
