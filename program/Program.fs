#light

//
// <<Hassan Murtaza>>
// U. of Illinois, Chicago
// CS 341, Fall 2018
// Project #05: Language prediction based on letter frequencies
//
// This program analyzes text from various languages, and computes
// letter frequencies.  These letter frequencies serve as a "barcode"
// that potentially identify the language when written.  This approach,
// and assignment, is inspired by the students and professor of CS 141,
// Fall 2018, at the U. of Illinois, Chicago.  Kudos to Prof Reed.
//


//
// explode:
//
// Given a string s, explodes the string into a list of characters.
// Example: explode "apple" => ['a';'p';'p';'l';'e']
//
let explode s =
  [for c in s -> c]



//
// implode
//
// The opposite of explode --- given a list of characters, returns
// the list as a string.
//
let implode L =
  let sb = System.Text.StringBuilder()
  List.iter (fun c -> ignore (sb.Append (c:char))) L
  sb.ToString()



//
// FileInput:
//
// Inputs text from a file line by line, returning this as a list
// of strings.  Each line is converted to lower-case.
//
let FileInput filename = 
  [ for line in System.IO.File.ReadLines(filename) -> line.ToLower() ]

// Takes a string of characters within a line and and increments count if the letter matches the letter passed
let rec _getCount letter L = 
 match L with
 | [] -> 0
 | e::rest -> if e = letter then 1 + _getCount letter rest
              else _getCount letter rest

//Takes a line and explodes it and returns a count of each line
let rec getCount letter L = 
 match L with
 | [] -> 0
 | e::rest -> (_getCount letter (explode e) ) + getCount letter rest
 

// Maps each language to the count of each letter within that file
let rec freqCount L = 
 match L with
 | [] -> []
 | e::rest -> 
  let X = FileInput e
  let xtail = List.tail X
  [((List.head (X)),[('a',getCount 'a' xtail);('b',getCount 'b' xtail);('c',getCount 'c' xtail);('d',getCount 'd' xtail);('e',getCount 'e' xtail);('f',getCount 'f' xtail);('g',getCount 'g' xtail);('h',getCount 'h' xtail);('i',getCount 'i' xtail);('j',getCount 'j' xtail);('k',getCount 'k' xtail);('l',getCount 'l' xtail);('m',getCount 'm' xtail);('n',getCount 'n' xtail);('o',getCount 'o' xtail);('p',getCount 'p' xtail);('q',getCount 'q' xtail);('r',getCount 'r' xtail);('s',getCount 's' xtail);('t',getCount 't' xtail);('u',getCount 'u' xtail);('v',getCount 'v' xtail);('w',getCount 'w' xtail);('x',getCount 'x' xtail);('y',getCount 'y' xtail);('z',getCount 'z' xtail)])] @ freqCount rest

// Using the tuples only gives the numbers of how often the letter appeared
let rec fn L1 L2 = 
 match L1 with
 | [] -> List.rev L2
 | (letter,freq)::rest -> fn rest (freq::L2)
  
// Maps the list to language with a list of just counts of each letter  
let rec langCount L L2=
 match L with
 | [] -> List.rev L2
 | (e,y)::rest -> langCount rest ((e, fn y [])::L2)
 
//  Prints each list element without the colons
let rec pL L1 =
 match L1 with
 | [] -> printfn ""
 | e::rest -> printf "%A " e
              pL rest

// Prints language with each letter count               
let rec printList L1 =
 match L1 with 
 | [] -> printfn ""
 | (e,y)::rest -> printf "%A: " e 
                  pL y
                  printList rest

// // GETTING CHARACTERS IN ORDER

//Returns the characters in order from high freq to low
let rec getCharOrder L1 = 
 let R = List.sortBy(fun(x,y) -> -y-1) L1
 List.map(fun(letter,count) -> letter) R
 
// returns the language with the order of char from high to low
let rec charCount L L2 = 
 match L with
 | [] -> List.rev L2
 | (e,y)::rest -> charCount rest ((e, getCharOrder y)::L2)
 
 // Prints the characters of each language
let rec pL2 L1 =
 match L1 with
 | [] -> printfn ""
 | e::rest -> printf "%c" e
              pL2 rest
 //Prints the language
let rec printList2 L1 =
 match L1 with 
 | [] -> printfn ""
 | (e,y)::rest -> printf "%A: " e 
                  pL2 y
                  printList2 rest
 
 // USER INPUT DETAILS AND INFO 
let rec userCount L = 
  let xtail = L
  [('a',getCount 'a' xtail);('b',getCount 'b' xtail);('c',getCount 'c' xtail);('d',getCount 'd' xtail);('e',getCount 'e' xtail);('f',getCount 'f' xtail);('g',getCount 'g' xtail);('h',getCount 'h' xtail);('i',getCount 'i' xtail);('j',getCount 'j' xtail);('k',getCount 'k' xtail);('l',getCount 'l' xtail);('m',getCount 'm' xtail);('n',getCount 'n' xtail);('o',getCount 'o' xtail);('p',getCount 'p' xtail);('q',getCount 'q' xtail);('r',getCount 'r' xtail);('s',getCount 's' xtail);('t',getCount 't' xtail);('u',getCount 'u' xtail);('v',getCount 'v' xtail);('w',getCount 'w' xtail);('x',getCount 'x' xtail);('y',getCount 'y' xtail);('z',getCount 'z' xtail)]
  
let rec printUser L =
 match L with
 | [] -> printfn ""
 | (e,y)::rest -> printf "%A " y
                  printUser rest
                  
             
// Adding positions to the letters 

// This will just get a list of letters and give back a list of letters with their positions
let rec _setPosition L1 L2 R = 
 match L1, L2 with
 | [],[] -> List.rev R
 | L1, [] -> List.rev R
 | [], L2 -> List.rev R
 | e1::rest1,e2::rest2 -> _setPosition rest1 rest2 ( (e1,e2)::R ) 

// setPosition will get a list in format of [(english,['e';'t';'p';'z';...])],[(finnish,[a;s;;d;f;f])]
let indices = [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25]
let rec setPosition L1 R = 
 match L1 with
 | [] -> List.rev R
 | (e,y)::rest -> setPosition rest ( ( (e, (_setPosition y indices []) ) ) :: R)
 
// Getting differences between userinput and each langauge

// Takes in two values and the threshold if the absolute difference is greater than the threshold 
// it will return the value otherwise 0
let rec getValue x y t = 
 let value = ( abs (x-y) )
 if (value > t) then value else 0
 
// returns the index of the matching "e" in L
let rec getIndex L e = 
 match L with
 | [] -> 0
 | (x,z)::rest -> if x = e then z else (getIndex rest e)

// Returns the value to languageDiff of the difference in indexs
let rec _languageDiff L1 L2 T R = 
 match L1 with
 | [] -> R
 | (e,y)::rest -> _languageDiff rest L2 T ( R + (getValue y (getIndex L2 e) T) )


// Gets a list of lanagues with their characters and position(L1), L2 is userinput chars with position, T is the threshold, R holds
// the list of each language and the difference of index positions
let rec languageDiff L1 L2 T R =
 match L1 with
 | [] -> List.rev R
 | (e,y)::rest -> languageDiff rest L2 T ( (e, _languageDiff y L2 T 0) :: R )
 
//
// UserInput:
//
// This function reads from the keyboard, line by line, until 
// # appears on a line by itself.  The lines are returned as
// a list of strings; each line is converted to lower-case.
//
// NOTE: if the first line of input is blank (i.e. the user 
// presses ENTER), then input is read from the file 'input.txt'.
// Helpful when testing.
//
let rec _UserInput input =
  let line = System.Console.ReadLine()
  match line with
  | "#" -> List.rev input
  |  _  -> _UserInput (line.ToLower() :: input)

let UserInput() =
  let firstLine = System.Console.ReadLine()
  match firstLine with
  | "" -> FileInput @"./input.txt"
  | _  -> _UserInput [firstLine.ToLower()]




// *********************************************************************** //
//
// Main:
//
[<EntryPoint>]
let main argv =
  printfn "** Training... **"
  printfn ""
  //
  let mainIndexs = [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25]
  let files = [ for filename in System.IO.Directory.GetFiles(@"./training") -> filename]
  let lanFreqs = (freqCount files)
  printfn "** Letter Frequency Counts (A->Z) **"
  let sortedFreqCount = ( List.sortBy(fun(x,y) -> x) lanFreqs )
  let r1 = langCount sortedFreqCount []
  printList r1
  //
  printfn "** Letter Frequency Order (High->Low) **"
  let r2 = charCount sortedFreqCount []
  printList2 r2
  
  let r2WithPos = (setPosition r2 [])
  // Here we get text from the user, analyze, and guess the language:
  //
  printfn "Please enter text, followed by # (default = 'input.txt')> "
  let text = UserInput()
  let userArr = userCount text
  printf "\"input\": "
  printUser userArr
  let userCharArr = getCharOrder userArr
  printf "\"input\": " 
  pL2 userCharArr
  let userArrWithPos = (_setPosition userCharArr mainIndexs [])
  printfn ""

  printf "Enter difference threshold (default = 4)> "
  printfn ""
  let s = System.Console.ReadLine()
  let threshold = if s = "" then 4 else int(s)
  let userAnalyze = (languageDiff r2WithPos userArrWithPos threshold [])
  let sortAnalyze = List.sortBy(fun(e,y) -> y ) userAnalyze
  printf "diffs: "
  printfn "%A" sortAnalyze
  printfn ""
  //
  let prediction = fst (List.head sortAnalyze)
  printfn "** Input language: %A" prediction
  printfn ""
  //
  0
