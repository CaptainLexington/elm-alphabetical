module Alphabetical exposing
    ( Options, SortMode(..), NumberSort(..)
    , sort, compare, indexSort, naturalSort
    )

{-| Alphabetical is a library for sorting English words and phrases the way editors and writers tend to sort them, not how computers do.

##A Note About Proper Nouns
Many proper nouns, such as people and place names, are very complicated to sort.

-Franklin Delano Roosevelt would be sorted under R, for Roosevelt (Delano being his middle name), but Andrew Lloyd Webber would be sorted under L (as his is an unhyphenated double-barreled surname).
-Korean film director Park Chan-Wook ought to be sorted under P, as Park is his family name.
-Lake Geneva, WI would be sorted under L, but the lake itself would be sorted under G.

For these reasons, no attempt is made to identify proper nouns or distingish between different categories of them.

Likewise, no attempt is made to alter the strings being sorted. A person's name is almost always formatted in an index as "Last, First Middle"; however, Alphabetical will need to have strings provided in this format to be sorted accordingly.


## Options

@docs Options, SortMode, NumberSort


## Comparitor

@docs sort, compare, indexSort, naturalSort

-}

import List
import Parser
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
    = Lexically
    | Numerically


{-|

  - `sortMode` lets you choose between sorting letter by letter or word by word
  - `initialNumberSort` sets a `NumberSort` for numbers encountered at the beginning of a string.
  - `internalNumberSort` sets a `NumberSort` for numbers encountered after at least some letters
  - When `years` is `True`, Alphabetical will attempt to sort four-digit numbers alphabetically, regardless of the values for `initialNumberSort` or `internalNumberSort` in English words the way they are normally pronounced as years, e.g. "1984" becomes "Nineteen Eighy Four" instead of "One Thousand, Nine Hundred Eighty Four"
  - When `romanNumberals` is `True`, it will attempt to sort strings that resemble Roman Numerals as Arabic numberals, sorted according to `initialNumberSort` or `internalNumberSort` as appropriate
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


indexSortOptions =
    { sortMode = WordByWord
    , initialNumberSort = Lexically
    , internalNumberSort = Numerically
    , years = True
    , romanNumerals = False
    , ignoreInitialArticle = True
    }


naturalSortOptions =
    { sortMode = LetterByLetter
    , initialNumberSort = Numerically
    , internalNumberSort = Numerically
    , years = False
    , romanNumerals = False
    , ignoreInitialArticle = False
    }


processForAll : String -> String
processForAll string =
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



-- Arabic Numerals
-- Numerals To English


joinToNumber : List String -> String
joinToNumber strings =
    let
        places =
            List.filter (\s -> s /= "") strings
    in
    String.join " " places


singleDigitToEnglish : String -> Result String String
singleDigitToEnglish number =
    case number of
        "0" ->
            Ok "zero"

        "1" ->
            Ok "one"

        "2" ->
            Ok "two"

        "3" ->
            Ok "three"

        "4" ->
            Ok "four"

        "5" ->
            Ok "five"

        "6" ->
            Ok "six"

        "7" ->
            Ok "seven"

        "8" ->
            Ok "eight"

        "9" ->
            Ok "nine"

        _ ->
            Err (String.concat [ "Single Digit Error: ", number ])


teensToEnglish : String -> Result String String
teensToEnglish number =
    case number of
        "10" ->
            Ok "ten"

        "11" ->
            Ok "eleven"

        "12" ->
            Ok "twelve"

        "13" ->
            Ok "thirteen"

        "14" ->
            Ok "fourteen"

        "15" ->
            Ok "fifteen"

        "16" ->
            Ok "sixteen"

        "17" ->
            Ok "seventeen"

        "18" ->
            Ok "eighteen"

        "19" ->
            Ok "nineteen"

        _ ->
            Err (String.concat [ "Teen Error: ", number ])


doubleDigitsToEnglish : String -> Result String String
doubleDigitsToEnglish tens =
    case tens of
        "0" ->
            Ok ""

        "2" ->
            Ok "twenty"

        "3" ->
            Ok "thirty"

        "4" ->
            Ok "forty"

        "5" ->
            Ok "fifty"

        "6" ->
            Ok "sixty"

        "7" ->
            Ok "seventy"

        "8" ->
            Ok "eighty"

        "9" ->
            Ok "ninety"

        _ ->
            Err "DoubleDigits Error"


powersOfTen : Int -> Result String String
powersOfTen power =
    case power of
        3 ->
            Ok "thousand"

        6 ->
            Ok "million"

        9 ->
            Ok "billion"

        12 ->
            Ok "trillion"

        _ ->
            Err "Powers of ten error!"


arabicToEnglish : String -> String
arabicToEnglish number =
    let
        digits =
            String.length number
    in
    if digits == 0 then
        ""

    else if number == String.repeat digits "0" then
        ""

    else if digits == 1 then
        case singleDigitToEnglish number of
            Ok word ->
                word

            Err errorMessage ->
                errorMessage

    else if digits == 2 then
        if String.startsWith "1" number then
            case teensToEnglish number of
                Ok word ->
                    word

                Err errorMessage ->
                    errorMessage

        else
            let
                tens =
                    String.left 1 number

                ones =
                    String.right 1 number
            in
            case doubleDigitsToEnglish tens of
                Ok word ->
                    joinToNumber [ word, arabicToEnglish ones ]

                Err errorMessage ->
                    errorMessage

    else if digits == 3 then
        let
            hundred =
                arabicToEnglish (String.left 1 number)

            tens =
                arabicToEnglish (String.right 2 number)
        in
        if hundred == "" || hundred == "0" then
            tens

        else
            joinToNumber [ hundred, "hundred", tens ]

    else if digits > 3 then
        let
            initialDigitCount =
                case modBy 3 digits of
                    0 ->
                        3

                    count ->
                        count

            initialDigits =
                String.left initialDigitCount number

            trailingDigits =
                String.right (digits - initialDigitCount) number

            magnitude =
                powersOfTen (String.length trailingDigits)
        in
        case magnitude of
            Ok power ->
                joinToNumber
                    [ arabicToEnglish initialDigits
                    , power
                    , arabicToEnglish trailingDigits
                    ]

            Err errorMessage ->
                errorMessage

    else
        String.concat [ "Parse Error: ", number ]


replaceDigitsWithEnglishWords : Bool -> String -> String
replaceDigitsWithEnglishWords internal =
    let
        digits =
            if internal then
                "\\d+"

            else
                "^\\d+"

        digitsRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString digits
    in
    Regex.replace digitsRegex (.match >> arabicToEnglish)



-- NUMERALS TO INTEGERS
-- With due credit to mcordova47/elm-natural-ordering


type Chunk
    = StringPart String
    | IntPart String



-- (TODO)


replaceDigitsWithNumericalValues : String -> String
replaceDigitsWithNumericalValues =
    identity


processForInitialNumberSort : NumberSort -> String -> String
processForInitialNumberSort numberSort =
    case numberSort of
        Lexically ->
            replaceDigitsWithEnglishWords False

        Numerically ->
            identity


processForInternalNumberSort : NumberSort -> String -> String
processForInternalNumberSort numberSort =
    case numberSort of
        Lexically ->
            replaceDigitsWithEnglishWords True

        Numerically ->
            identity


arabicToEnglishYear : String -> String
arabicToEnglishYear year =
    let
        century =
            String.left 2 year

        decadeAndYear =
            String.right 2 year
    in
    if String.slice 1 3 year == "00" then
        arabicToEnglish year

    else if String.right 2 year == "00" then
        joinToNumber
            [ arabicToEnglish century
            , "hundred"
            ]

    else
        joinToNumber
            [ arabicToEnglish century
            , arabicToEnglish decadeAndYear
            ]


replaceDigitsWithEnglishYear : String -> String
replaceDigitsWithEnglishYear =
    let
        year =
            "\\b\\d{4}\\b"

        yearRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString year
    in
    Regex.replace yearRegex (.match >> arabicToEnglishYear)


processForYears : Bool -> String -> String
processForYears years =
    if years then
        replaceDigitsWithEnglishYear

    else
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
        |> processForAll
        -- First, perform whitespace-dependent processing!
        |> processForInitialArticles ignoreInitialArticle
        |> processForYears years
        -- Convert Roman Numerals to Numbers (TODO)
        |> processForRomanNumerals romanNumerals
        -- Translate into either English or Elm Integers
        |> processForInitialNumberSort initialNumberSort
        |> processForInternalNumberSort internalNumberSort
        -- Now, handle whitespace appropriately
        |> processForSortMode sortMode



-- Exported Sort functions!


{-| compare is a comparitor like `Basics.compare` that can be passed to `List.sortWith` and similar functions. It is the more flexible option
-}
compare : Options -> String -> String -> Order
compare options stringA stringB =
    let
        processedA =
            process options stringA

        processedB =
            process options stringB
    in
    Basics.compare processedA processedB


{-| sort expects and returns a `List` of `Strings`, and so is less flexible than `compare`, but in addition to being more convenient, it is also more efficient. Using `compare`, each computation is done several times per entry; using `sort`, each computation is done only once per entry.
-}
sort : Options -> List String -> List String
sort options strings =
    let
        processedStrings =
            List.map (\s -> Tuple.pair s (process options s)) strings
    in
    List.map Tuple.first (List.sortBy Tuple.second processedStrings)


{-| `indexSort` sorts according to how the index of a standard English-language non-fiction book would be sorted, and is probably the sort you want if you are sorting a list of English proper nouns, like movie titles or band names.
-}
indexSort : List String -> List String
indexSort =
    sort indexSortOptions


{-| `naturalSort` sorts according to our "natural" expecations for strings like filenames, for instance [as described at Coding Horror](https://blog.codinghorror.com/sorting-for-humans-natural-sort-order/).
-}
naturalSort : List String -> List String
naturalSort =
    sort naturalSortOptions
