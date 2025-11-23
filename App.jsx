import React, { useState, useEffect, useMemo, useRef } from 'react';
import { Brain, Globe, FlaskConical, Flag, Languages, ChevronLeft, Info, Check, X, Utensils, Users, Shield, ArrowRight, Terminal, Lock, Unlock, Star, Map, Wifi, Crown, ScrollText, Castle, Radio, Search, Glasses } from 'lucide-react';

// --- DATASETS ---

const PI_100 = "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679";

const ELEMENTS = [
    { n: 1, s: "H", name: "Hydrogen" }, { n: 2, s: "He", name: "Helium" }, { n: 3, s: "Li", name: "Lithium" },
    { n: 4, s: "Be", name: "Beryllium" }, { n: 5, s: "B", name: "Boron" }, { n: 6, s: "C", name: "Carbon" },
    { n: 7, s: "N", name: "Nitrogen" }, { n: 8, s: "O", name: "Oxygen" }, { n: 9, s: "F", name: "Fluorine" },
    { n: 10, s: "Ne", name: "Neon" }, { n: 11, s: "Na", name: "Sodium" }, { n: 12, s: "Mg", name: "Magnesium" },
    { n: 13, s: "Al", name: "Aluminium" }, { n: 14, s: "Si", name: "Silicon" }, { n: 15, s: "P", name: "Phosphorus" },
    { n: 16, s: "S", name: "Sulfur" }, { n: 17, s: "Cl", name: "Chlorine" }, { n: 18, s: "Ar", name: "Argon" },
    { n: 19, s: "K", name: "Potassium" }, { n: 20, s: "Ca", name: "Calcium" }, { n: 26, s: "Fe", name: "Iron" },
    { n: 29, s: "Cu", name: "Copper" }, { n: 47, s: "Ag", name: "Silver" }, { n: 79, s: "Au", name: "Gold" },
    { n: 80, s: "Hg", name: "Mercury" }, { n: 82, s: "Pb", name: "Lead" }, { n: 92, s: "U", name: "Uranium" }
];

const NATO_PHONETIC = [
    { l: "A", w: "Alpha" }, { l: "B", w: "Bravo" }, { l: "C", w: "Charlie" }, { l: "D", w: "Delta" },
    { l: "E", w: "Echo" }, { l: "F", w: "Foxtrot" }, { l: "G", w: "Golf" }, { l: "H", w: "Hotel" },
    { l: "I", w: "India" }, { l: "J", w: "Juliett" }, { l: "K", w: "Kilo" }, { l: "L", w: "Lima" },
    { l: "M", w: "Mike" }, { l: "N", w: "November" }, { l: "O", w: "Oscar" }, { l: "P", w: "Papa" },
    { l: "Q", w: "Quebec" }, { l: "R", w: "Romeo" }, { l: "S", w: "Sierra" }, { l: "T", w: "Tango" },
    { l: "U", w: "Uniform" }, { l: "V", w: "Victor" }, { l: "W", w: "Whiskey" }, { l: "X", w: "X-ray" },
    { l: "Y", w: "Yankee" }, { l: "Z", w: "Zulu" }
];

const MORSE_CODE = [
    { l: "A", c: ".-" }, { l: "B", c: "-..." }, { l: "C", c: "-.-." }, { l: "D", c: "-.." },
    { l: "E", c: "." }, { l: "F", c: "..-." }, { l: "G", c: "--." }, { l: "H", c: "...." },
    { l: "I", c: ".." }, { l: "J", c: ".---" }, { l: "K", c: "-.-" }, { l: "L", c: ".-.." },
    { l: "M", c: "--" }, { l: "N", c: "-." }, { l: "O", c: "---" }, { l: "P", c: ".--." },
    { l: "Q", c: "--.-" }, { l: "R", c: ".-." }, { l: "S", c: "..." }, { l: "T", c: "-" },
    { l: "U", c: "..-" }, { l: "V", c: "...-" }, { l: "W", c: ".--" }, { l: "X", c: "-..-" },
    { l: "Y", c: "-.--" }, { l: "Z", c: "--.." },
    { l: "1", c: ".----" }, { l: "2", c: "..---" }, { l: "3", c: "...--" }, { l: "4", c: "....-" },
    { l: "5", c: "....." }, { l: "6", c: "-...." }, { l: "7", c: "--..." }, { l: "8", c: "---.." },
    { l: "9", c: "----." }, { l: "0", c: "-----" }
];

const BRAILLE_CODE = [
    { l: "A", c: "⠁" }, { l: "B", c: "⠃" }, { l: "C", c: "⠉" }, { l: "D", c: "⠙" },
    { l: "E", c: "⠑" }, { l: "F", c: "⠋" }, { l: "G", c: "⠛" }, { l: "H", c: "⠓" },
    { l: "I", c: "⠊" }, { l: "J", c: "⠚" }, { l: "K", c: "⠅" }, { l: "L", c: "⠇" },
    { l: "M", c: "⠍" }, { l: "N", c: "⠝" }, { l: "O", c: "⠕" }, { l: "P", c: "⠏" },
    { l: "Q", c: "⠟" }, { l: "R", c: "⠗" }, { l: "S", c: "⠎" }, { l: "T", c: "⠞" },
    { l: "U", c: "⠥" }, { l: "V", c: "⠧" }, { l: "W", c: "⠺" }, { l: "X", c: "⠭" },
    { l: "Y", c: "⠽" }, { l: "Z", c: "⠵" }
];

const FLAGS_WITH_CAPITALS = [
    { c: "Europe", n: "France", code: "fr", capital: "Paris" }, { c: "Europe", n: "Germany", code: "de", capital: "Berlin" }, 
    { c: "Europe", n: "Italy", code: "it", capital: "Rome" }, { c: "Europe", n: "Spain", code: "es", capital: "Madrid" }, 
    { c: "Europe", n: "United Kingdom", code: "gb", capital: "London" }, { c: "Europe", n: "Sweden", code: "se", capital: "Stockholm" },
    { c: "Europe", n: "Ukraine", code: "ua", capital: "Kyiv" }, { c: "Asia", n: "Japan", code: "jp", capital: "Tokyo" }, 
    { c: "Asia", n: "China", code: "cn", capital: "Beijing" }, { c: "Asia", n: "India", code: "in", capital: "New Delhi" }, 
    { c: "Asia", n: "South Korea", code: "kr", capital: "Seoul" }, { c: "Asia", n: "Vietnam", code: "vn", capital: "Hanoi" },
    { c: "North America", n: "USA", code: "us", capital: "Washington D.C." }, { c: "North America", n: "Canada", code: "ca", capital: "Ottawa" }, 
    { c: "North America", n: "Mexico", code: "mx", capital: "Mexico City" }, { c: "South America", n: "Brazil", code: "br", capital: "Brasilia" }, 
    { c: "South America", n: "Argentina", code: "ar", capital: "Buenos Aires" }, { c: "South America", n: "Chile", code: "cl", capital: "Santiago" },
    { c: "Africa", n: "South Africa", code: "za", capital: "Pretoria" }, { c: "Africa", n: "Nigeria", code: "ng", capital: "Abuja" }, 
    { c: "Africa", n: "Egypt", code: "eg", capital: "Cairo" }, { c: "Africa", n: "Kenya", code: "ke", capital: "Nairobi" }, 
    { c: "Oceania", n: "Australia", code: "au", capital: "Canberra" }, { c: "Oceania", n: "New Zealand", code: "nz", capital: "Wellington" },
    { c: "Europe", n: "Netherlands", code: "nl", capital: "Amsterdam" }, { c: "Europe", n: "Belgium", code: "be", capital: "Brussels" }, 
    { c: "Europe", n: "Portugal", code: "pt", capital: "Lisbon" }, { c: "Asia", n: "Thailand", code: "th", capital: "Bangkok" }, 
    { c: "Asia", n: "Indonesia", code: "id", capital: "Jakarta" }, { c: "Asia", n: "Malaysia", code: "my", capital: "Kuala Lumpur" },
    { c: "Asia", n: "Singapore", code: "sg", capital: "Singapore" }, { c: "Europe", n: "Poland", code: "pl", capital: "Warsaw" }, 
    { c: "Europe", n: "Austria", code: "at", capital: "Vienna" }, { c: "Europe", n: "Switzerland", code: "ch", capital: "Bern" }, 
    { c: "Europe", n: "Ireland", code: "ie", capital: "Dublin" }, { c: "Europe", n: "Norway", code: "no", capital: "Oslo" }
];

const FLAGS = FLAGS_WITH_CAPITALS.map(({c, n, code}) => ({c, n, code}));

const US_PRESIDENTS = [
    { n: 1, name: "George Washington" }, { n: 2, name: "John Adams" }, { n: 3, name: "Thomas Jefferson" },
    { n: 4, name: "James Madison" }, { n: 5, name: "James Monroe" }, { n: 6, name: "John Quincy Adams" },
    { n: 7, name: "Andrew Jackson" }, { n: 8, name: "Martin Van Buren" }, { n: 9, name: "William Henry Harrison" },
    { n: 10, name: "John Tyler" }, { n: 11, name: "James K. Polk" }, { n: 12, name: "Zachary Taylor" },
    { n: 13, name: "Millard Fillmore" }, { n: 14, name: "Franklin Pierce" }, { n: 15, name: "James Buchanan" },
    { n: 16, name: "Abraham Lincoln" }, { n: 17, name: "Andrew Johnson" }, { n: 18, name: "Ulysses S. Grant" },
    { n: 19, name: "Rutherford B. Hayes" }, { n: 20, name: "James A. Garfield" }, { n: 21, name: "Chester A. Arthur" },
    { n: 22, name: "Grover Cleveland" }, { n: 23, name: "Benjamin Harrison" }, { n: 24, name: "Grover Cleveland" },
    { n: 25, name: "William McKinley" }, { n: 26, name: "Theodore Roosevelt" }, { n: 27, name: "William Howard Taft" },
    { n: 28, name: "Woodrow Wilson" }, { n: 29, name: "Warren G. Harding" }, { n: 30, name: "Calvin Coolidge" },
    { n: 31, name: "Herbert Hoover" }, { n: 32, name: "Franklin D. Roosevelt" }, { n: 33, name: "Harry S. Truman" },
    { n: 34, name: "Dwight D. Eisenhower" }, { n: 35, name: "John F. Kennedy" }, { n: 36, name: "Lyndon B. Johnson" },
    { n: 37, name: "Richard Nixon" }, { n: 38, name: "Gerald Ford" }, { n: 39, name: "Jimmy Carter" },
    { n: 40, name: "Ronald Reagan" }, { n: 41, name: "George H. W. Bush" }, { n: 42, name: "Bill Clinton" },
    { n: 43, name: "George W. Bush" }, { n: 44, name: "Barack Obama" }, { n: 45, name: "Donald Trump" },
    { n: 46, name: "Joe Biden" }
];

const BRITISH_MONARCHS = [
    "William I", "William II", "Henry I", "Stephen", "Henry II", "Richard I", "John", "Henry III", 
    "Edward I", "Edward II", "Edward III", "Richard II", "Henry IV", "Henry V", "Henry VI", "Edward IV", 
    "Edward V", "Richard III", "Henry VII", "Henry VIII", "Edward VI", "Mary I", "Elizabeth I", 
    "James I", "Charles I", "Charles II", "James II", "William III & Mary II", "Anne", "George I", 
    "George II", "George III", "George IV", "William IV", "Victoria", "Edward VII", "George V", 
    "Edward VIII", "George VI", "Elizabeth II", "Charles III"
];

const GREETINGS = [
    { l: "Spanish", w: "Hola" }, { l: "French", w: "Bonjour" }, { l: "German", w: "Hallo" },
    { l: "Italian", w: "Ciao" }, { l: "Portuguese", w: "Olá" }, { l: "Japanese", w: "Konnichiwa" },
    { l: "Chinese (Mandarin)", w: "Ni Hao" }, { l: "Russian", w: "Privet" }, { l: "Arabic", w: "Marhaba" },
    { l: "Hindi", w: "Namaste" }, { l: "Korean", w: "Annyeong" }, { l: "Dutch", w: "Hallo" },
    { l: "Swedish", w: "Hej" }, { l: "Swahili", w: "Jambo" }, { l: "Turkish", w: "Merhaba" },
    { l: "Hebrew", w: "Shalom" }, { l: "Greek", w: "Yassas" }, { l: "Thai", w: "Sawasdee" },
    { l: "Polish", w: "Czesc" }, { l: "Vietnamese", w: "Xin Chao" }, { l: "Hawaiian", w: "Aloha" },
    { l: "Indonesian", w: "Halo" }, { l: "Czech", w: "Ahoj" }, { l: "Norwegian", w: "Hei" },
    { l: "Danish", w: "Hej" }, { l: "Finnish", w: "Hei" }, { l: "Hungarian", w: "Szia" },
    { l: "Romanian", w: "Salut" }, { l: "Bulgarian", w: "Zdravei" }, { l: "Croatian", w: "Bok" }
];

const DISHES = [
    { c: "Italy", d: "Pizza", code: "it" }, { c: "Italy", d: "Pasta", code: "it" }, { c: "Japan", d: "Sushi", code: "jp" }, { c: "Japan", d: "Ramen", code: "jp" }, 
    { c: "Spain", d: "Paella", code: "es" }, { c: "Germany", d: "Sauerbraten", code: "de" }, { c: "Germany", d: "Bratwurst", code: "de" },
    { c: "France", d: "Pot-au-Feu", code: "fr" }, { c: "France", d: "Crepes", code: "fr" }, { c: "United Kingdom", d: "Fish and Chips", code: "gb" }, 
    { c: "USA", d: "Hamburger", code: "us" }, { c: "USA", d: "Apple Pie", code: "us" }, { c: "Canada", d: "Poutine", code: "ca" },
    { c: "Mexico", d: "Tacos", code: "mx" }, { c: "Mexico", d: "Mole", code: "mx" }, { c: "Brazil", d: "Feijoada", code: "br" }, 
    { c: "Argentina", d: "Asado", code: "ar" }, { c: "India", d: "Curry", code: "in" }, { c: "India", d: "Biryani", code: "in" },
    { c: "Thailand", d: "Pad Thai", code: "th" }, { c: "Vietnam", d: "Pho", code: "vn" }, { c: "South Korea", d: "Kimchi", code: "kr" }, { c: "South Korea", d: "Bibimbap", code: "kr" },
    { c: "China", d: "Peking Duck", code: "cn" }, { c: "China", d: "Dim Sum", code: "cn" }, { c: "Turkey", d: "Kebab", code: "tr" }, 
    { c: "Greece", d: "Moussaka", code: "gr" }, { c: "Russia", d: "Borscht", code: "ru" }, { c: "Poland", d: "Pierogi", code: "pl" },
    { c: "Sweden", d: "Meatballs", code: "se" }, { c: "Switzerland", d: "Fondue", code: "ch" }, { c: "Belgium", d: "Moules-Frites", code: "be" }, 
    { c: "Austria", d: "Wiener Schnitzel", code: "at" }, { c: "Hungary", d: "Goulash", code: "hu" }, { c: "Peru", d: "Ceviche", code: "pe" }, 
    { c: "Australia", d: "Meat Pie", code: "au" }, { c: "South Africa", d: "Bobotie", code: "za" }, { c: "Egypt", d: "Koshary", code: "eg" },
    { c: "Morocco", d: "Tagine", code: "ma" }, { c: "Lebanon", d: "Tabbouleh", code: "lb" }, { c: "Portugal", d: "Bacalhau", code: "pt" },
    { c: "Jamaica", d: "Ackee and Saltfish", code: "jm" }
];

const POPULATION = [
    { c: "China", p: 1412 }, { c: "India", p: 1428 }, { c: "USA", p: 335 }, { c: "Indonesia", p: 277 },
    { c: "Pakistan", p: 240 }, { c: "Nigeria", p: 224 }, { c: "Brazil", p: 216 }, { c: "Bangladesh", p: 173 },
    { c: "Russia", p: 144 }, { c: "Mexico", p: 128 }, { c: "Japan", p: 123 }, { c: "Ethiopia", p: 126 },
    { c: "Philippines", p: 117 }, { c: "Egypt", p: 113 }, { c: "Vietnam", p: 99 }, { c: "DR Congo", p: 102 },
    { c: "Turkey", p: 86 }, { c: "Iran", p: 89 }, { c: "Germany", p: 84 }, { c: "Thailand", p: 72 },
    { c: "United Kingdom", p: 68 }, { c: "France", p: 68 }, { c: "Italy", p: 59 }, { c: "South Africa", p: 60 },
    { c: "Tanzania", p: 67 }, { c: "Myanmar", p: 54 }, { c: "South Korea", p: 52 }, { c: "Colombia", p: 52 },
    { c: "Kenya", p: 55 }, { c: "Spain", p: 48 }, { c: "Argentina", p: 46 }, { c: "Ukraine", p: 37 }
];

const ALLIANCES = [
    {
        id: "NATO",
        name: "NATO",
        full: "North Atlantic Treaty Organization",
        members: [
            "Albania", "Belgium", "Bulgaria", "Canada", "Croatia", "Czech Republic", "Denmark", "Estonia", 
            "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Italy", "Latvia", "Lithuania", 
            "Luxembourg", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", 
            "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Turkey", "United Kingdom", "United States"
        ]
    },
    {
        id: "G7",
        name: "G7",
        full: "Group of Seven",
        members: ["Canada", "France", "Germany", "Italy", "Japan", "United Kingdom", "United States"]
    },
    {
        id: "BRICS",
        name: "BRICS+",
        full: "Brazil, Russia, India, China, South Africa & New Members",
        members: [
            "Brazil", "Russia", "India", "China", "South Africa", 
            "Egypt", "Ethiopia", "Iran", "Saudi Arabia", "United Arab Emirates"
        ]
    },
    {
        id: "ASEAN",
        name: "ASEAN",
        full: "Association of Southeast Asian Nations",
        members: [
            "Brunei", "Cambodia", "Indonesia", "Laos", "Malaysia", "Myanmar", 
            "Philippines", "Singapore", "Thailand", "Vietnam"
        ]
    },
    {
        id: "OPEC",
        name: "OPEC",
        full: "Organization of the Petroleum Exporting Countries",
        members: [
            "Algeria", "Congo", "Equatorial Guinea", "Gabon", "Iran", "Iraq", "Kuwait", 
            "Libya", "Nigeria", "Saudi Arabia", "United Arab Emirates", "Venezuela"
        ]
    },
    {
        id: "EU",
        name: "EU",
        full: "European Union",
        members: [
            "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", 
            "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", 
            "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", 
            "Spain", "Sweden"
        ]
    }
];

const EMPIRES = [
    {
        id: "BRITISH",
        name: "British Empire",
        full: "At its peak, the largest empire in history (c. 1920).",
        members: [
            "United Kingdom", "Ireland", "United States", "Canada", "Australia", "New Zealand", "India", "Pakistan", 
            "Bangladesh", "South Africa", "Nigeria", "Kenya", "Egypt", "Sudan", "Ghana", "Jamaica", "Malaysia", 
            "Singapore", "Myanmar", "Sri Lanka", "Hong Kong"
        ]
    },
    {
        id: "ROMAN",
        name: "Roman Empire",
        full: "The great empire of antiquity (c. 117 AD).",
        members: [
            "Italy", "France", "Spain", "Portugal", "United Kingdom", "Belgium", "Netherlands", "Switzerland", 
            "Austria", "Hungary", "Romania", "Bulgaria", "Greece", "Turkey", "Syria", "Lebanon", "Israel", 
            "Egypt", "Libya", "Tunisia", "Algeria", "Morocco"
        ]
    },
    {
        id: "MONGOL",
        name: "Mongol Empire",
        full: "The largest contiguous land empire (c. 1270).",
        members: [
            "Mongolia", "China", "Russia", "Ukraine", "Belarus", "Kazakhstan", "Uzbekistan", "Turkmenistan", 
            "Kyrgyzstan", "Tajikistan", "Iran", "Iraq", "Afghanistan", "Pakistan", "South Korea", "North Korea"
        ]
    },
    {
        id: "SPANISH",
        name: "Spanish Empire",
        full: "The first global empire (c. 1790).",
        members: [
            "Spain", "Mexico", "Cuba", "Dominican Republic", "Puerto Rico", "Guatemala", "Honduras", 
            "El Salvador", "Nicaragua", "Costa Rica", "Panama", "Colombia", "Venezuela", "Ecuador", "Peru", 
            "Bolivia", "Paraguay", "Chile", "Argentina", "Philippines", "Equatorial Guinea"
        ]
    },
    {
        id: "OTTOMAN",
        name: "Ottoman Empire",
        full: "A superpower spanning three continents (c. 1683).",
        members: [
            "Turkey", "Greece", "Bulgaria", "Romania", "North Macedonia", "Albania", "Serbia", "Bosnia and Herzegovina", 
            "Syria", "Lebanon", "Israel", "Jordan", "Iraq", "Kuwait", "Saudi Arabia", "Yemen", "Egypt", "Libya", 
            "Tunisia", "Algeria"
        ]
    }
];

const CODE_SNIPPETS = [
    { l: "Python", c: `print("Hello, World!")` },
    { l: "Java", c: `System.out.println("Hello, World!");` },
    { l: "C", c: `printf("Hello, World!");` },
    { l: "C++", c: `std::cout << "Hello, World!";` },
    { l: "C#", c: `Console.WriteLine("Hello, World!");` },
    { l: "JavaScript", c: `console.log("Hello, World!");` },
    { l: "TypeScript", c: `console.log("Hello, World!");` },
    { l: "Ruby", c: `puts "Hello, World!"` },
    { l: "PHP", c: `echo "Hello, World!";` },
    { l: "Swift", c: `print("Hello, World!")` },
    { l: "Go", c: `fmt.Println("Hello, World!")` },
    { l: "Rust", c: `println!("Hello, World!");` },
    { l: "Kotlin", c: `println("Hello, World!")` },
    { l: "Perl", c: `print "Hello, World!\\n";` },
    { l: "Lua", c: `print("Hello, World!")` },
    { l: "R", c: `print("Hello, World!")` },
    { l: "Bash", c: `echo "Hello, World!"` },
    { l: "PowerShell", c: `Write-Host "Hello, World!"` },
    { l: "SQL", c: `SELECT 'Hello, World!';` },
    { l: "HTML", c: `<h1>Hello, World!</h1>` },
    { l: "Haskell", c: `putStrLn "Hello, World!"` },
    { l: "Julia", c: `println("Hello, World!")` },
    { l: "Dart", c: `print('Hello, World!');` },
    { l: "Elixir", c: `IO.puts("Hello, World!")` },
    { l: "Scala", c: `println("Hello, World!")` },
    { l: "F#", c: `printfn "Hello, World!"` },
    { l: "COBOL", c: `DISPLAY "Hello, World!".` },
    { l: "Fortran", c: `print *, "Hello, World!"` },
    { l: "Pascal", c: `WriteLn('Hello, World!');` },
    { l: "BASIC", c: `PRINT "Hello, World!"` },
    { l: "Lisp", c: `(print "Hello, World!")` },
    { l: "MATLAB", c: `disp('Hello, World!')` },
    { l: "Objective-C", c: `NSLog(@"Hello, World!");` },
    { l: "Groovy", c: `println "Hello, World!"` },
    { l: "Visual Basic", c: `Console.WriteLine("Hello, World!")` }
];

// Re-calculated "authentic" relative coordinates (0-100)
const CONSTELLATIONS = [
    {
        name: "Ursa Major (Big Dipper)",
        desc: "The Great Bear. The Big Dipper is an asterism within it.",
        stars: [
            [80, 20], // Dubhe
            [80, 40], // Merak
            [55, 42], // Phecda
            [52, 28], // Megrez
            [35, 33], // Alioth
            [22, 38], // Mizar
            [5, 55]   // Alkaid
        ],
        lines: [[0, 1], [1, 2], [2, 3], [3, 0], [3, 4], [4, 5], [5, 6]]
    },
    {
        name: "Orion",
        desc: "The Hunter. Recognizable by the belt of three stars.",
        stars: [
            [25, 15], // Betelgeuse (Left Shoulder)
            [75, 20], // Bellatrix (Right Shoulder)
            [42, 48], // Alnitak (Belt)
            [50, 48], // Alnilam (Belt)
            [58, 48], // Mintaka (Belt)
            [28, 85], // Saiph (Left Knee)
            [72, 80]  // Rigel (Right Knee)
        ],
        lines: [[0, 2], [1, 4], [2, 3], [3, 4], [2, 5], [4, 6], [5, 6]]
    },
    {
        name: "Cassiopeia",
        desc: "The Queen. The distinctive 'W' or 'M' shape.",
        stars: [
            [10, 60], // Caph
            [25, 30], // Schedar
            [50, 50], // Gamma Cas
            [70, 40], // Ruchbah
            [90, 70]  // Segin
        ],
        lines: [[0, 1], [1, 2], [2, 3], [3, 4]]
    },
    {
        name: "Leo",
        desc: "The Lion. Featuring the 'Sickle' shape.",
        stars: [
            [75, 70], // Regulus
            [60, 50], // Eta Leonis
            [55, 30], // Algieba
            [40, 25], // Adhafera
            [25, 35], // Rasalas
            [15, 60], // Denebola (Tail)
            [30, 65], // Zosma
            [35, 50]  // Chertan
        ],
        lines: [[0, 1], [1, 2], [2, 3], [3, 4], [2, 7], [7, 6], [6, 5], [0, 7]] 
    },
    {
        name: "Crux",
        desc: "The Southern Cross. Small but iconic.",
        stars: [
            [50, 10], // Gacrux (Top)
            [50, 90], // Acrux (Bottom)
            [20, 45], // Mimosa (Left)
            [80, 40], // Delta (Right)
            [65, 65]  // Epsilon (Small)
        ],
        lines: [[0, 1], [2, 3]]
    },
    {
        name: "Cygnus",
        desc: "The Swan or Northern Cross. Flies along the Milky Way.",
        stars: [
            [50, 10], // Deneb (Tail/Top)
            [50, 50], // Sadr (Center)
            [50, 90], // Albireo (Head/Bottom)
            [15, 60], // Delta Cyg (Wing)
            [85, 40]  // Epsilon Cyg (Wing)
        ],
        lines: [[0, 1], [1, 2], [3, 1], [1, 4]]
    },
    {
        name: "Scorpius",
        desc: "The Scorpion. A winding 'J' shape.",
        stars: [
            [85, 20], // Graffias
            [80, 25], // Dschubba
            [70, 35], // Antares (Heart)
            [60, 55], // Tau Sco
            [50, 75], // Epsilon Sco
            [40, 85], // Mu Sco
            [30, 90], // Zeta Sco
            [20, 80], // Sargas
            [15, 65], // Shaula (Stinger)
            [10, 60]  // Lesath (Stinger)
        ],
        lines: [[0, 1], [1, 2], [2, 3], [3, 4], [4, 5], [5, 6], [6, 7], [7, 8], [8, 9]]
    },
    {
        name: "Gemini",
        desc: "The Twins, Castor and Pollux.",
        stars: [
            [30, 10], // Castor
            [70, 15], // Pollux
            [35, 40], // Mebsuta
            [65, 45], // Wasat
            [30, 80], // Alhena
            [70, 85]  // Mekbuda
        ],
        lines: [[0, 2], [2, 4], [1, 3], [3, 5], [0, 1]] // Simplified bodies + connection head
    }
];

const TLD_DATA = [
    { country: "Tuvalu", tld: ".tv", fact: "Highly marketable — .tv is commercially popular worldwide for television- and streaming-related domains." },
    { country: "British Indian Ocean Territory", tld: ".io", fact: "Widely used by tech startups — .io is prized for 'input/output' associations despite being a territory ccTLD." },
    { country: "Anguilla", tld: ".ai", fact: "Popular with AI companies and projects because 'AI' matches the extension." },
    { country: "Montenegro", tld: ".me", fact: "Marketed for personal sites and call-to-action domains (e.g., about.me)." },
    { country: "Guernsey", tld: ".gg", fact: "Adopted by gaming communities and streamers because 'gg' means 'good game'." },
    { country: "Libya", tld: ".ly", fact: "Commonly used for clever domain hacks (e.g., bit.ly)." },
    { country: "Soviet Union", tld: ".su", fact: "The .su domain (Soviet Union) still exists and is occasionally used despite the country no longer existing." },
    { country: "Federated States of Micronesia", tld: ".fm", fact: "Often used by radio stations and podcasts because 'FM' evokes radio broadcasting." },
    { country: "Armenia", tld: ".am", fact: "Used for radio/amateur radio or podcast branding because 'AM' matches amplitude modulation." },
    { country: "Colombia", tld: ".co", fact: "Marketed globally as an alternative to .com (‘co’ for company or commercial)." },
    { country: "Germany", tld: ".de", fact: "One of the largest ccTLD spaces in the world by registration count." },
    { country: "United Kingdom", tld: ".uk", fact: "The UK primarily uses .uk rather than the older .gb, and .uk is widely recognized." },
    { country: "United States", tld: ".us", fact: "The .us domain supports locality-based registrations and historically required a connection to the U.S." },
    { country: "China", tld: ".cn", fact: "One of the largest ccTLD registries by number of registered domains." },
    { country: "Russia", tld: ".ru", fact: "A very large ccTLD with extensive usage in Russia." },
    { country: "Saudi Arabia", tld: ".sa", fact: "A major ccTLD in the Middle East with regulated registration policies." },
    { country: "United Arab Emirates", tld: ".ae", fact: "Widely used in the UAE; .ae is an established regional ccTLD." },
    { country: "Egypt", tld: ".eg", fact: "Egypt’s ccTLD is used primarily by local entities and organizations." },
    { country: "Nigeria", tld: ".ng", fact: "One of Africa’s larger ccTLD spaces by registrations." },
    { country: "Kenya", tld: ".ke", fact: "A prominent East African ccTLD with growing digital adoption." },
    { country: "Ghana", tld: ".gh", fact: "Used widely within Ghana and for local services." },
    { country: "Morocco", tld: ".ma", fact: "Morocco’s national ccTLD serving the local internet community." },
    { country: "Poland", tld: ".pl", fact: "A major European ccTLD with extensive domestic usage." },
    { country: "Austria", tld: ".at", fact: "Used domestically and sometimes for domain hacks (e.g., words ending in 'at')." },
    { country: "Belgium", tld: ".be", fact: "Belgium’s ccTLD is popular due to multilingual usage (FR/NL/DE)." },
    { country: "Czech Republic", tld: ".cz", fact: "A widely used ccTLD in Central Europe." },
    { country: "Slovakia", tld: ".sk", fact: "Used across Slovakia and for Slovak-language sites." },
    { country: "Hungary", tld: ".hu", fact: "Hungary’s ccTLD, used widely by domestic entities." },
    { country: "Romania", tld: ".ro", fact: "Romania’s ccTLD has broad domestic adoption." },
    { country: "Bulgaria", tld: ".bg", fact: "Used in Bulgaria and managed by a national registry." },
    { country: "Croatia", tld: ".hr", fact: "Croatia’s ccTLD is used for national and business sites." },
    { country: "Slovenia", tld: ".si", fact: "Si is also the Slovenian word for 'you are' — sometimes used in wordplay." },
    { country: "Serbia", tld: ".rs", fact: "An active ccTLD in the Balkans used for local domains." },
    { country: "Ukraine", tld: ".ua", fact: "A major ccTLD in Eastern Europe with many registrations." }
];

// Helper: Normalizes text for loose matching (removes accents, case, common variations)
const normalize = (str) => {
    return str.toLowerCase().trim()
        .replace(/&/g, "and")
        .replace(/\./g, "")
        .normalize("NFD").replace(/[\u0300-\u036f]/g, ""); // remove accents
};

const COUNTRY_ALIASES = {
    "usa": "United States", "us": "United States", "america": "United States",
    "uk": "United Kingdom", "britain": "United Kingdom", "great britain": "United Kingdom",
    "uae": "United Arab Emirates", "emirates": "United Arab Emirates",
    "czechia": "Czech Republic",
    "turkiye": "Turkey",
    "holland": "Netherlands",
    "congo republic": "Congo", "republic of the congo": "Congo",
    "dr congo": "Democratic Republic of the Congo", "drc": "Democratic Republic of the Congo",
    "st kitts": "Saint Kitts and Nevis", "st lucia": "Saint Lucia",
    "myanmar": "Myanmar", "burma": "Myanmar"
};

// --- HELPER HOOK FOR DECK LOGIC ---
function useDeck(items) {
    const [deck, setDeck] = useState(() => [...items].sort(() => 0.5 - Math.random()));
    const [current, setCurrent] = useState(null);

    const draw = () => {
        if (deck.length === 0) {
            const newDeck = [...items].sort(() => 0.5 - Math.random());
            const next = newDeck.pop();
            setDeck(newDeck);
            setCurrent(next);
        } else {
            const newDeck = [...deck];
            const next = newDeck.pop();
            setDeck(newDeck);
            setCurrent(next);
        }
    };

    useEffect(() => { if (!current) draw(); }, []);

    return { current, draw, remaining: deck.length };
}

// --- COMPONENTS ---

const Card = ({ children, className = "", onClick }) => (
    <div onClick={onClick} className={`bg-gray-800 border border-gray-700 rounded-xl p-6 shadow-lg hover:shadow-xl transition-all duration-300 ${className}`}>
        {children}
    </div>
);

const Button = ({ children, onClick, variant = "primary", className = "", disabled = false }) => {
    const base = "px-4 py-2 rounded-lg font-semibold transition-all duration-200 flex items-center justify-center gap-2";
    const variants = {
        primary: "bg-blue-600 hover:bg-blue-500 text-white shadow-lg hover:shadow-blue-500/20",
        secondary: "bg-gray-700 hover:bg-gray-600 text-gray-200",
        success: "bg-green-600 hover:bg-green-500 text-white",
        danger: "bg-red-600 hover:bg-red-500 text-white",
        outline: "border-2 border-gray-600 hover:border-gray-400 text-gray-300"
    };
    return (
        <button 
            onClick={onClick} 
            disabled={disabled}
            className={`${base} ${variants[variant]} ${disabled ? 'opacity-50 cursor-not-allowed' : ''} ${className}`}
        >
            {children}
        </button>
    );
};

// --- GAMES ---

const PiGame = ({ onBack }) => {
    const [input, setInput] = useState("");
    const [error, setError] = useState(false);
    const [revealed, setRevealed] = useState(false);

    const handleChange = (e) => {
        const val = e.target.value;
        if (val.length > 0 && val[val.length - 1] !== PI_100[val.length - 1]) {
            setError(true);
            setTimeout(() => setError(false), 500);
        } else {
            setInput(val);
            setError(false);
        }
    };
    const progress = (input.length / PI_100.length) * 100;
    return (
        <div className="max-w-2xl mx-auto animate-fade-in">
            <div className="flex justify-between items-center mb-6">
                <h2 className="text-3xl font-bold text-blue-400">Pi Master</h2>
                <div className="text-right"><div className="text-4xl font-bold font-mono">{input.length}</div><div className="text-xs text-gray-400">DIGITS</div></div>
            </div>
            <Card className="mb-6 relative overflow-hidden">
                <div className={`text-center transition-all duration-300 ${error ? 'animate-wiggle text-red-500' : 'text-white'}`}>
                    <textarea autoFocus value={input} onChange={handleChange} className="w-full bg-transparent text-3xl md:text-5xl font-mono outline-none resize-none text-center h-32 md:h-48 tracking-widest" placeholder="3.14..." spellCheck="false" />
                </div>
                <div className="absolute bottom-0 left-0 h-1 bg-gray-700 w-full"><div className="h-full bg-blue-500 transition-all duration-300" style={{ width: `${progress}%` }}></div></div>
            </Card>
            <div className="flex gap-4 justify-center">
                <Button variant="secondary" onClick={() => setRevealed(!revealed)}>{revealed ? "Hide Cheat Sheet" : "Peek"}</Button>
                <Button variant="danger" onClick={() => setInput("")}>Reset</Button>
            </div>
            {revealed && <div className="mt-6 p-4 bg-gray-800 rounded-lg border border-gray-700 font-mono text-sm break-all leading-relaxed text-gray-400">{PI_100}</div>}
        </div>
    );
};

const PeriodicGame = () => {
    const [current, setCurrent] = useState(null);
    const [options, setOptions] = useState([]);
    const [score, setScore] = useState(0);
    const [mode, setMode] = useState('symbol');
    const [feedback, setFeedback] = useState(null);

    const generateQuestion = () => {
        const target = ELEMENTS[Math.floor(Math.random() * ELEMENTS.length)];
        const distractors = ELEMENTS.filter(e => e.n !== target.n).sort(() => 0.5 - Math.random()).slice(0, 3);
        setCurrent(target);
        setOptions([target, ...distractors].sort(() => 0.5 - Math.random()));
        setFeedback(null);
    };
    useEffect(() => { generateQuestion(); }, [mode]);
    const handleAnswer = (selected) => {
        if (feedback) return;
        if (selected.n === current.n) {
            setScore(s => s + 1);
            setFeedback('correct');
            setTimeout(generateQuestion, 800);
        } else {
            setScore(0);
            setFeedback('wrong');
            setTimeout(generateQuestion, 1500);
        }
    };
    if (!current) return null;
    return (
        <div className="max-w-xl mx-auto animate-fade-in">
            <div className="flex justify-between items-center mb-8">
                <div className="flex gap-2">
                    <button onClick={() => setMode('symbol')} className={`px-3 py-1 rounded text-sm ${mode === 'symbol' ? 'bg-green-600' : 'bg-gray-700'}`}>Symbol</button>
                    <button onClick={() => setMode('number')} className={`px-3 py-1 rounded text-sm ${mode === 'number' ? 'bg-green-600' : 'bg-gray-700'}`}>Number</button>
                </div>
                <div className="bg-gray-800 px-4 py-2 rounded-full border border-gray-700 text-green-400 font-bold">Streak: {score}</div>
            </div>
            <div className="flex justify-center mb-8">
                <div className="w-40 h-40 bg-gray-100 text-gray-900 rounded-lg flex flex-col items-center justify-center shadow-2xl border-4 border-white relative">
                    <span className="absolute top-2 left-3 text-lg font-bold text-gray-500">{current.n}</span>
                    <span className="text-6xl font-bold">{current.s}</span>
                    {mode === 'number' && <span className="text-sm font-semibold mt-1 text-gray-600">?</span>}
                </div>
            </div>
            <div className="grid grid-cols-2 gap-4">
                {options.map((opt, idx) => (
                    <button key={idx} onClick={() => handleAnswer(opt)} className={`p-4 rounded-lg font-bold text-lg transition-all duration-200 border-2 ${feedback === 'correct' && opt.n === current.n ? 'bg-green-600 border-green-400' : feedback === 'wrong' && opt.n === current.n ? 'bg-green-600 border-green-400' : feedback === 'wrong' ? 'bg-red-900/50 border-red-600 opacity-50' : 'bg-gray-800 border-gray-700 hover:bg-gray-700 hover:border-gray-500'}`}>{opt.name}</button>
                ))}
            </div>
        </div>
    );
};

const FlagGame = () => {
    const { current, draw, remaining } = useDeck(FLAGS);
    const [options, setOptions] = useState([]);
    const [score, setScore] = useState(0);
    const [feedback, setFeedback] = useState(null);

    useEffect(() => {
        if (!current) return;
        const distractors = FLAGS.filter(f => f.n !== current.n).sort(() => 0.5 - Math.random()).slice(0, 3);
        setOptions([current, ...distractors].sort(() => 0.5 - Math.random()));
        setFeedback(null);
    }, [current]);

    const handleAnswer = (opt) => {
        if (feedback) return;
        if (opt.n === current.n) {
            setScore(s => s + 1);
            setFeedback('correct');
            setTimeout(draw, 800);
        } else {
            setScore(0);
            setFeedback('wrong');
            setTimeout(draw, 1500);
        }
    };

    if (!current) return <div className="text-center">Loading Deck...</div>;

    return (
        <div className="max-w-2xl mx-auto animate-fade-in">
             <div className="text-center text-xs text-gray-500 mb-4">Cards remaining in deck: {remaining}</div>
            <div className="flex flex-col items-center justify-center mb-8 h-48">
                <img src={`https://flagcdn.com/w320/${current.code}.png`} alt="Flag" className="h-full rounded-lg shadow-2xl object-contain" />
                <div className="mt-4 text-gray-500 text-sm uppercase tracking-widest font-semibold">{score} Correct in a row</div>
            </div>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                {options.map((opt, i) => (
                    <button key={i} onClick={() => handleAnswer(opt)} className={`p-4 rounded-xl text-lg font-medium transition-all border-2 ${feedback === 'correct' && opt.n === current.n ? 'bg-green-500/20 border-green-500 text-green-400' : feedback === 'wrong' && opt.n === current.n ? 'bg-green-500/20 border-green-500 text-green-400' : feedback === 'wrong' && opt.n !== current.n ? 'opacity-30' : 'bg-gray-800 border-gray-700 hover:bg-gray-700'}`}>{opt.n}</button>
                ))}
            </div>
        </div>
    );
};

const GeoGame = () => {
    const [current, setCurrent] = useState(null);
    const [answer, setAnswer] = useState("");
    const [reveal, setReveal] = useState(false);

    const next = () => {
        const data = FLAGS_WITH_CAPITALS;
        const random = data[Math.floor(Math.random() * data.length)];
        setCurrent(random);
        setAnswer("");
        setReveal(false);
    };

    useEffect(() => { next(); }, []);

    const check = () => {
        const target = current.capital;
        if (answer.toLowerCase().trim() === target.toLowerCase()) {
            setReveal(true);
        }
    };

    if (!current) return null;

    return (
        <div className="max-w-lg mx-auto animate-fade-in">
            <Card className="text-center py-12 mb-6">
                <div className="text-gray-400 text-sm uppercase tracking-wider mb-2">Country</div>
                
                <div className="flex justify-center mb-6">
                    <img 
                        src={`https://flagcdn.com/w160/${current.code}.png`} 
                        alt="flag" 
                        className="h-24 rounded shadow-lg object-contain"
                    />
                </div>

                <h2 className="text-4xl font-bold text-white mb-8">{current.n}</h2>
                
                {!reveal ? (
                    <div className="space-y-4">
                        <p className="text-blue-300">What is the capital city?</p>
                        <input 
                            type="text" 
                            value={answer}
                            onChange={(e) => setAnswer(e.target.value)}
                            onKeyDown={(e) => e.key === 'Enter' && check()}
                            className="bg-gray-900 border border-gray-600 rounded px-4 py-2 text-center text-white w-full max-w-xs outline-none focus:border-blue-500"
                            placeholder="Type answer..."
                        />
                        <div className="flex gap-2 justify-center">
                            <Button onClick={check} variant="success" className="w-full max-w-xs">Submit</Button>
                            <Button onClick={() => setReveal(true)} variant="secondary">Give Up</Button>
                        </div>
                    </div>
                ) : (
                    <div className="animate-fade-in">
                        <div className="text-green-400 text-2xl font-bold mb-4">{current.capital}</div>
                        <Button onClick={next} variant="primary">Next Country</Button>
                    </div>
                )}
            </Card>
        </div>
    );
};

const LanguageGame = () => {
    const { current, draw, remaining } = useDeck(GREETINGS);
    const [isFlipped, setIsFlipped] = useState(false);
    const [isAnimating, setIsAnimating] = useState(false);

    const next = () => {
        if (isAnimating) return;
        setIsFlipped(false);
        setIsAnimating(true);
        setTimeout(() => {
            draw();
            setIsAnimating(false);
        }, 300);
    };

    if (!current) return <div>Loading...</div>;

    return (
        <div className="max-w-md mx-auto h-96 perspective-1000 animate-fade-in">
            <div className="text-center mb-4 text-gray-400">Cards remaining: {remaining}</div>
            <div className="relative w-full h-full cursor-pointer transition-transform duration-500 transform-style-3d" style={{ transform: isFlipped ? 'rotateY(180deg)' : '' }} onClick={() => setIsFlipped(!isFlipped)}>
                <div className="absolute w-full h-full bg-gradient-to-br from-purple-600 to-blue-800 rounded-2xl flex flex-col items-center justify-center p-8 backface-hidden shadow-2xl border border-white/10">
                    <h3 className="text-xl text-purple-200 mb-2">How do you say Hello in</h3>
                    <h2 className="text-4xl font-bold text-white">{current.l}?</h2>
                </div>
                <div className="absolute w-full h-full bg-gray-800 rounded-2xl flex flex-col items-center justify-center p-8 backface-hidden shadow-2xl border border-gray-700" style={{ transform: 'rotateY(180deg)' }}>
                    <h2 className="text-5xl font-bold text-green-400 mb-8">{current.w}</h2>
                    <Button onClick={(e) => { e.stopPropagation(); next(); }} variant="outline">Next Language</Button>
                </div>
            </div>
        </div>
    );
};

// 6. NATIONAL DISH GAME (Expanded & Upgraded)
const DishGame = () => {
    const { current, draw } = useDeck(DISHES);
    const [isFlipped, setIsFlipped] = useState(false);
    const [isAnimating, setIsAnimating] = useState(false);
    const [showCountryFirst, setShowCountryFirst] = useState(true);
    const [input, setInput] = useState("");
    const [score, setScore] = useState(0);
    const [feedback, setFeedback] = useState(null);

    const next = () => {
        if (isAnimating) return;
        setIsFlipped(false);
        setIsAnimating(true);
        setTimeout(() => {
            draw();
            setShowCountryFirst(Math.random() > 0.5);
            setInput("");
            setFeedback(null);
            setIsAnimating(false);
        }, 300);
    };

    useEffect(() => {
        if (current) setShowCountryFirst(Math.random() > 0.5);
    }, [current]);

    const checkAnswer = () => {
        const target = showCountryFirst ? current.d : current.c;
        const cleanInput = input.toLowerCase().trim();
        const cleanTarget = target.toLowerCase();
        
        if (cleanTarget.includes(cleanInput) && cleanInput.length > 3) {
            setScore(s => s + 1);
            setFeedback("correct");
            setIsFlipped(true);
        } else {
            setFeedback("wrong");
        }
    };

    if (!current) return <div>Loading...</div>;

    const frontLabel = showCountryFirst ? "What is the national dish of" : "Which country's national dish is";
    const frontValue = showCountryFirst ? current.c : current.d;
    const backLabel = showCountryFirst ? "National Dish" : "Country";
    const backValue = showCountryFirst ? current.d : current.c;

    return (
        <div className="max-w-md mx-auto h-[550px] perspective-1000 animate-fade-in flex flex-col">
            <div className="text-center mb-4 text-green-400 font-bold">Score: {score}</div>
            <div className="relative flex-1 cursor-pointer transition-transform duration-500 transform-style-3d" style={{ transform: isFlipped ? 'rotateY(180deg)' : '' }}>
                {/* Front Side */}
                <div className="absolute w-full h-full bg-gradient-to-br from-orange-600 to-red-800 rounded-2xl flex flex-col items-center justify-between p-8 backface-hidden shadow-2xl border border-white/10 text-center">
                    <div className="flex-1 flex flex-col justify-center items-center w-full">
                        <Utensils className="text-orange-200 mb-4" size={32}/>
                        <h3 className="text-xl text-orange-100 mb-2">{frontLabel}</h3>
                        
                        {/* Show flag if showing country name */}
                        {showCountryFirst && (
                             <img 
                                src={`https://flagcdn.com/w160/${current.code}.png`} 
                                alt="flag" 
                                className="h-16 mb-4 rounded shadow-md object-contain"
                            />
                        )}

                        <h2 className="text-4xl font-bold text-white mb-2">{frontValue}?</h2>
                    </div>
                    
                    {/* Input Area (Only on Front) */}
                    <div className="w-full mt-4" onClick={(e) => e.stopPropagation()}>
                        <input 
                            type="text" 
                            value={input} 
                            onChange={(e) => setInput(e.target.value)}
                            className={`w-full p-3 rounded-lg bg-black/30 border text-white placeholder-orange-200/50 outline-none mb-2 ${feedback === 'wrong' ? 'border-red-400 animate-wiggle' : 'border-white/20'}`}
                            placeholder="Type guess..."
                            onKeyDown={(e) => e.key === 'Enter' && checkAnswer()}
                        />
                        <div className="flex gap-2">
                            <Button onClick={checkAnswer} variant="success" className="flex-1">Guess</Button>
                            <Button onClick={() => setIsFlipped(true)} variant="secondary" className="flex-1">Flip / Give Up</Button>
                        </div>
                    </div>
                </div>

                {/* Back Side */}
                <div className="absolute w-full h-full bg-gray-800 rounded-2xl flex flex-col items-center justify-center p-8 backface-hidden shadow-2xl border border-gray-700" style={{ transform: 'rotateY(180deg)' }}>
                    <h3 className="text-sm text-gray-400 mb-2">{backLabel}</h3>
                    
                    {/* Show flag if revealing country */}
                    {!showCountryFirst && (
                         <img 
                            src={`https://flagcdn.com/w160/${current.code}.png`} 
                            alt="flag" 
                            className="h-20 mb-6 rounded shadow-lg object-contain"
                        />
                    )}

                    <h2 className="text-4xl font-bold text-green-400 mb-8 text-center">{backValue}</h2>
                    <Button onClick={(e) => { e.stopPropagation(); next(); }} variant="outline">Next Card</Button>
                </div>
            </div>
        </div>
    );
};

const PopulationGame = () => {
    const { current, draw, remaining } = useDeck(POPULATION);
    const [options, setOptions] = useState([]);
    const [score, setScore] = useState(0);
    const [feedback, setFeedback] = useState(null);

    useEffect(() => {
        if (!current) return;
        const correct = current.p;
        const d1 = Math.round(correct * (0.5 + Math.random() * 0.4));
        const d2 = Math.round(correct * (1.1 + Math.random() * 0.5));
        const d3 = Math.round(correct * (1.5 + Math.random() * 1.0));
        
        const opts = [
            { val: correct, correct: true },
            { val: d1 === correct ? d1 + 10 : d1, correct: false },
            { val: d2 === correct ? d2 + 10 : d2, correct: false },
            { val: d3 === correct ? d3 + 10 : d3, correct: false }
        ].sort(() => 0.5 - Math.random());
        
        setOptions(opts);
        setFeedback(null);
    }, [current]);

    const handleAnswer = (isCorrect) => {
        if (feedback) return;
        if (isCorrect) {
            setScore(s => s + 1);
            setFeedback('correct');
            setTimeout(draw, 1000);
        } else {
            setScore(0);
            setFeedback('wrong');
            setTimeout(draw, 2000);
        }
    };

    if (!current) return <div>Loading...</div>;

    return (
        <div className="max-w-md mx-auto animate-fade-in">
             <div className="text-center text-xs text-gray-500 mb-4">Countries remaining: {remaining}</div>
            <Card className="text-center mb-6">
                <Users className="mx-auto text-blue-400 mb-2" size={32}/>
                <div className="text-gray-400 text-sm mb-2">Population to the nearest million</div>
                <h2 className="text-4xl font-bold text-white mb-2">{current.c}</h2>
                <div className="text-xs text-green-400 font-bold tracking-widest mt-4">STREAK: {score}</div>
            </Card>

            <div className="grid grid-cols-2 gap-4">
                {options.map((opt, i) => (
                    <button 
                        key={i} 
                        onClick={() => handleAnswer(opt.correct)}
                        className={`p-4 rounded-xl text-xl font-bold transition-all border-2 
                            ${feedback === 'correct' && opt.correct ? 'bg-green-600 border-green-400 text-white' : 
                              feedback === 'wrong' && opt.correct ? 'bg-green-600 border-green-400 text-white' :
                              feedback === 'wrong' && !opt.correct ? 'opacity-30' : 
                              'bg-gray-800 border-gray-600 hover:bg-gray-700'}`}
                    >
                        {opt.val} M
                    </button>
                ))}
            </div>
            {feedback === 'wrong' && <div className="text-center mt-4 text-red-400 font-bold animate-bounce">Incorrect! It was {current.p} Million.</div>}
        </div>
    );
};

// 8. ALLIANCE GAME (Menu & Save Added)
const AllianceGame = () => {
    const [view, setView] = useState('MENU'); // MENU, GAME
    const [selectedAlliance, setSelectedAlliance] = useState(null);
    const [input, setInput] = useState("");
    const [guessed, setGuessed] = useState(new Set());
    const [flash, setFlash] = useState(false);
    const [completedAlliances, setCompletedAlliances] = useState(() => {
        // Load from local storage if available
        try {
            const saved = localStorage.getItem('completedAlliances');
            return saved ? JSON.parse(saved) : [];
        } catch (e) { return []; }
    });

    useEffect(() => {
        localStorage.setItem('completedAlliances', JSON.stringify(completedAlliances));
    }, [completedAlliances]);

    const startAlliance = (alliance) => {
        setSelectedAlliance(alliance);
        setGuessed(new Set());
        setInput("");
        setView('GAME');
    };

    const checkAnswer = (val) => {
        const cleanVal = normalize(val);
        
        // Find match in members
        const match = selectedAlliance.members.find(m => {
            const normM = normalize(m);
            if (normM === cleanVal) return true;
            if (COUNTRY_ALIASES[cleanVal] === m) return true;
            return false;
        });

        if (match && !guessed.has(match)) {
            const newGuessed = new Set(guessed);
            newGuessed.add(match);
            setGuessed(newGuessed);
            setInput("");
            
            // Check completion
            if (newGuessed.size === selectedAlliance.members.length) {
                if (!completedAlliances.includes(selectedAlliance.id)) {
                    setCompletedAlliances([...completedAlliances, selectedAlliance.id]);
                }
            }
        } else if (match && guessed.has(match)) {
            setFlash(true);
            setTimeout(() => setFlash(false), 300);
        }
    };

    if (view === 'MENU') {
        return (
            <div className="max-w-4xl mx-auto animate-fade-in">
                <h2 className="text-3xl font-bold text-center mb-8 text-blue-400">Select Alliance</h2>
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                    {ALLIANCES.map(a => {
                        const isComplete = completedAlliances.includes(a.id);
                        return (
                            <button 
                                key={a.id}
                                onClick={() => startAlliance(a)}
                                className={`p-6 rounded-xl text-left border transition-all hover:scale-105 relative overflow-hidden
                                    ${isComplete ? 'bg-green-900/30 border-green-500/50' : 'bg-gray-800 border-gray-700 hover:border-blue-500'}`}
                            >
                                <div className="flex justify-between items-start">
                                    <div>
                                        <div className="font-black text-2xl mb-1">{a.name}</div>
                                        <div className="text-xs text-gray-400">{a.members.length} Members</div>
                                    </div>
                                    {isComplete ? <Lock size={20} className="text-green-500" /> : <Unlock size={20} className="text-gray-600" />}
                                </div>
                                {isComplete && <div className="absolute bottom-0 left-0 w-full h-1 bg-green-500"></div>}
                            </button>
                        )
                    })}
                </div>
            </div>
        );
    }

    // GAME VIEW
    const total = selectedAlliance.members.length;
    const remaining = total - guessed.size;

    return (
        <div className="max-w-4xl mx-auto animate-fade-in">
            <button onClick={() => setView('MENU')} className="mb-4 flex items-center text-gray-400 hover:text-white"><ChevronLeft size={16}/> Alliances</button>
            <div className="text-center mb-8">
                <h2 className="text-5xl font-black text-blue-500 mb-2 tracking-tight">{selectedAlliance.name}</h2>
                <p className="text-gray-400 text-lg">{selectedAlliance.full}</p>
                <div className="mt-4 text-sm font-bold bg-gray-800 inline-block px-4 py-1 rounded-full text-gray-300">
                    {guessed.size} / {total} Members
                </div>
            </div>

            <div className="max-w-md mx-auto mb-8">
                <input 
                    autoFocus
                    type="text" 
                    value={input}
                    onChange={(e) => {
                        setInput(e.target.value);
                        checkAnswer(e.target.value);
                    }}
                    className={`w-full bg-gray-800 border-2 border-gray-600 rounded-xl px-6 py-4 text-xl text-center outline-none focus:border-blue-500 transition-all ${flash ? 'border-red-500' : ''}`}
                    placeholder="Type a country..."
                />
            </div>

            <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-3">
                {selectedAlliance.members.map((m, i) => {
                    const isGuessed = guessed.has(m);
                    return (
                        <div 
                            key={i} 
                            className={`h-16 flex items-center justify-center rounded-lg text-sm font-bold transition-all duration-500 px-2 text-center
                                ${isGuessed ? 'bg-green-600 text-white scale-100' : 'bg-gray-800 text-transparent scale-95 border border-gray-700'}`}
                        >
                            {isGuessed ? m : ""}
                        </div>
                    );
                })}
            </div>

            {remaining === 0 && (
                <div className="mt-12 text-center animate-fade-in">
                    <h3 className="text-3xl font-bold text-green-400 mb-4">Alliance Complete!</h3>
                    <Button onClick={() => setView('MENU')} variant="primary" className="mx-auto text-lg px-8 py-3">
                        Back to Menu <ArrowRight className="ml-2" />
                    </Button>
                </div>
            )}
        </div>
    );
};

// --- EMPIRE GAME ---
const EmpireGame = () => {
    const [view, setView] = useState('MENU');
    const [selectedEmpire, setSelectedEmpire] = useState(null);
    const [input, setInput] = useState("");
    const [guessed, setGuessed] = useState(new Set());
    const [flash, setFlash] = useState(false);
    const [completedEmpires, setCompletedEmpires] = useState(() => {
        try {
            const saved = localStorage.getItem('completedEmpires');
            return saved ? JSON.parse(saved) : [];
        } catch (e) { return []; }
    });

    useEffect(() => {
        localStorage.setItem('completedEmpires', JSON.stringify(completedEmpires));
    }, [completedEmpires]);

    const startEmpire = (empire) => {
        setSelectedEmpire(empire);
        setGuessed(new Set());
        setInput("");
        setView('GAME');
    };

    const checkAnswer = (val) => {
        const cleanVal = normalize(val);
        
        const match = selectedEmpire.members.find(m => {
            const normM = normalize(m);
            if (normM === cleanVal) return true;
            if (COUNTRY_ALIASES[cleanVal] === m) return true;
            return false;
        });

        if (match && !guessed.has(match)) {
            const newGuessed = new Set(guessed);
            newGuessed.add(match);
            setGuessed(newGuessed);
            setInput("");
            
            if (newGuessed.size === selectedEmpire.members.length) {
                if (!completedEmpires.includes(selectedEmpire.id)) {
                    setCompletedEmpires([...completedEmpires, selectedEmpire.id]);
                }
            }
        } else if (match && guessed.has(match)) {
            setFlash(true);
            setTimeout(() => setFlash(false), 300);
        }
    };

    if (view === 'MENU') {
        return (
            <div className="max-w-4xl mx-auto animate-fade-in">
                <h2 className="text-3xl font-bold text-center mb-8 text-yellow-500">Select Empire</h2>
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                    {EMPIRES.map(e => {
                        const isComplete = completedEmpires.includes(e.id);
                        return (
                            <button 
                                key={e.id}
                                onClick={() => startEmpire(e)}
                                className={`p-6 rounded-xl text-left border transition-all hover:scale-105 relative overflow-hidden
                                    ${isComplete ? 'bg-yellow-900/30 border-yellow-500/50' : 'bg-gray-800 border-gray-700 hover:border-yellow-500'}`}
                            >
                                <div className="flex justify-between items-start">
                                    <div>
                                        <div className="font-black text-2xl mb-1 text-yellow-100">{e.name}</div>
                                        <div className="text-xs text-gray-400">{e.members.length} Modern Territories</div>
                                    </div>
                                    {isComplete ? <Lock size={20} className="text-yellow-500" /> : <Unlock size={20} className="text-gray-600" />}
                                </div>
                                {isComplete && <div className="absolute bottom-0 left-0 w-full h-1 bg-yellow-500"></div>}
                            </button>
                        )
                    })}
                </div>
            </div>
        );
    }

    const total = selectedEmpire.members.length;
    const remaining = total - guessed.size;

    return (
        <div className="max-w-4xl mx-auto animate-fade-in">
            <button onClick={() => setView('MENU')} className="mb-4 flex items-center text-gray-400 hover:text-white"><ChevronLeft size={16}/> Empires</button>
            <div className="text-center mb-8">
                <h2 className="text-5xl font-black text-yellow-500 mb-2 tracking-tight">{selectedEmpire.name}</h2>
                <p className="text-gray-400 text-lg">{selectedEmpire.full}</p>
                <div className="mt-4 text-sm font-bold bg-gray-800 inline-block px-4 py-1 rounded-full text-gray-300">
                    {guessed.size} / {total} Territories
                </div>
            </div>

            <div className="max-w-md mx-auto mb-8">
                <input 
                    autoFocus
                    type="text" 
                    value={input}
                    onChange={(e) => {
                        setInput(e.target.value);
                        checkAnswer(e.target.value);
                    }}
                    className={`w-full bg-gray-800 border-2 border-gray-600 rounded-xl px-6 py-4 text-xl text-center outline-none focus:border-yellow-500 transition-all ${flash ? 'border-red-500' : ''}`}
                    placeholder="Type a modern country..."
                />
            </div>

            <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-3">
                {selectedEmpire.members.map((m, i) => {
                    const isGuessed = guessed.has(m);
                    return (
                        <div 
                            key={i} 
                            className={`h-16 flex items-center justify-center rounded-lg text-sm font-bold transition-all duration-500 px-2 text-center
                                ${isGuessed ? 'bg-yellow-600 text-black scale-100' : 'bg-gray-800 text-transparent scale-95 border border-gray-700'}`}
                        >
                            {isGuessed ? m : ""}
                        </div>
                    );
                })}
            </div>

            {remaining === 0 && (
                <div className="mt-12 text-center animate-fade-in">
                    <h3 className="text-3xl font-bold text-yellow-400 mb-4">Empire Conquered!</h3>
                    <Button onClick={() => setView('MENU')} variant="primary" className="mx-auto text-lg px-8 py-3 bg-yellow-600 hover:bg-yellow-500 border-none text-black">
                        Back to Menu <ArrowRight className="ml-2" />
                    </Button>
                </div>
            )}
        </div>
    );
};

// 9. CODE GAME (Added Selection & Case Warning)
const CodeGame = () => {
    const [selectedLang, setSelectedLang] = useState(null);
    const [input, setInput] = useState("");
    const [feedback, setFeedback] = useState(null);
    const [revealed, setRevealed] = useState(false);
    const [warning, setWarning] = useState("");

    const check = () => {
        if (!input.trim()) return;
        
        if (input.trim() === selectedLang.c) {
            setFeedback('correct');
            setWarning("");
            // Could trigger next or just show success
        } else if (input.trim().toLowerCase() === selectedLang.c.toLowerCase()) {
            // Case sensitive match failed but content correct
            setWarning("Code is case sensitive!");
            setFeedback('wrong');
        } else {
            setFeedback('wrong');
            setWarning("");
        }
        
        if (input.trim() === selectedLang.c) {
             setTimeout(() => {
                setFeedback(null);
                // In a full app, maybe mark as done or go back
             }, 1500);
        } else {
             setTimeout(() => setFeedback(null), 1000);
        }
    };

    if (!selectedLang) {
        return (
            <div className="max-w-4xl mx-auto animate-fade-in">
                <h2 className="text-3xl font-bold text-center mb-8 text-emerald-400">Select Language</h2>
                <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-4">
                    {CODE_SNIPPETS.map((s, i) => (
                        <button 
                            key={i} 
                            onClick={() => { setSelectedLang(s); setInput(""); setRevealed(false); setWarning(""); }}
                            className="p-4 bg-gray-800 border border-gray-700 rounded-lg hover:border-emerald-500 transition-all text-left"
                        >
                            <div className="font-mono font-bold text-emerald-400">{s.l}</div>
                        </button>
                    ))}
                </div>
            </div>
        )
    }

    return (
        <div className="max-w-2xl mx-auto animate-fade-in">
            <button onClick={() => setSelectedLang(null)} className="mb-4 flex items-center text-gray-400 hover:text-white"><ChevronLeft size={16}/> Languages</button>
            
            <div className="text-center mb-8">
                <h2 className="text-4xl font-bold text-emerald-400 mb-2">{selectedLang.l}</h2>
                <p className="text-gray-400">Type "Hello, World!"</p>
            </div>

            <Card className="mb-6 bg-black/50 border-gray-700">
                <div className="font-mono text-lg md:text-xl text-green-400 p-4 min-h-[100px] flex items-center justify-center text-center break-all">
                    {revealed ? selectedLang.c : (
                        <span className="text-gray-600 select-none">{'// Code hidden'}</span>
                    )}
                </div>
            </Card>

            <div className="relative">
                <input 
                    autoFocus
                    type="text" 
                    value={input}
                    onChange={(e) => setInput(e.target.value)}
                    onKeyDown={(e) => e.key === 'Enter' && check()}
                    className={`w-full bg-gray-800 border-2 rounded-xl px-6 py-4 text-xl font-mono outline-none transition-all
                        ${feedback === 'correct' ? 'border-green-500' : feedback === 'wrong' ? 'border-red-500' : 'border-gray-600 focus:border-emerald-500'}`}
                    placeholder="Type code here..."
                    spellCheck="false"
                />
                {feedback === 'correct' && (
                    <div className="absolute right-4 top-1/2 -translate-y-1/2 text-green-500"><Check /></div>
                )}
            </div>
            
            {warning && <div className="text-center mt-2 text-yellow-400 font-mono text-sm">{warning}</div>}

            <div className="flex gap-4 mt-6 justify-center">
                <Button onClick={check} variant="success" className="w-32 bg-emerald-600 hover:bg-emerald-500">Run</Button>
                <Button onClick={() => setRevealed(!revealed)} variant="secondary">
                    {revealed ? "Hide Answer" : "Peek Answer"}
                </Button>
            </div>
        </div>
    );
};

// --- NEW CONSTELLATION GAME ---

const ConstellationRenderer = ({ stars, lines, size = 300, animated = false }) => {
    return (
        <svg width={size} height={size} viewBox="0 0 100 100" className="overflow-visible">
            {/* Draw Lines */}
            {lines.map((l, i) => {
                const start = stars[l[0]];
                const end = stars[l[1]];
                return (
                    <line 
                        key={i} 
                        x1={start[0]} y1={start[1]} 
                        x2={end[0]} y2={end[1]} 
                        stroke="rgba(255,255,255,0.3)" 
                        strokeWidth="1"
                        strokeDasharray={animated ? "100" : "0"}
                        className={animated ? "animate-draw-line" : ""}
                    />
                );
            })}
            
            {/* Draw Stars */}
            {stars.map((s, i) => (
                <circle 
                    key={i} 
                    cx={s[0]} cy={s[1]} 
                    r={2} 
                    fill="white" 
                    className={`filter drop-shadow-glow ${animated ? 'animate-twinkle' : ''}`}
                    style={{ animationDelay: `${Math.random() * 2}s` }}
                />
            ))}
        </svg>
    );
};

const ConstellationGame = () => {
    const { current, draw, remaining } = useDeck(CONSTELLATIONS);
    const [reveal, setReveal] = useState(false);
    const [viewMode, setViewMode] = useState('GUESS'); // GUESS, MAP

    if (viewMode === 'MAP') {
        return (
            <div className="fixed inset-0 bg-gray-950 z-[100] overflow-hidden flex flex-col animate-fade-in">
                <div className="absolute inset-0 overflow-auto p-8">
                     {/* Starry Background */}
                    <div className="absolute inset-0 pointer-events-none">
                        {[...Array(50)].map((_, i) => (
                            <div 
                                key={i}
                                className="absolute bg-white rounded-full opacity-50 animate-twinkle"
                                style={{
                                    top: `${Math.random() * 100}%`,
                                    left: `${Math.random() * 100}%`,
                                    width: `${Math.random() * 3}px`,
                                    height: `${Math.random() * 3}px`,
                                    animationDuration: `${1 + Math.random() * 3}s`
                                }}
                            />
                        ))}
                    </div>

                    <div className="relative z-10 max-w-6xl mx-auto">
                        <div className="flex justify-between items-center mb-8">
                             <h2 className="text-3xl font-bold text-yellow-100">Star Map</h2>
                             <Button onClick={() => setViewMode('GUESS')} variant="secondary">Close Map</Button>
                        </div>
                        
                        <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
                            {CONSTELLATIONS.map((c, idx) => (
                                <div key={idx} className="bg-white/5 border border-white/10 rounded-xl p-6 hover:bg-white/10 transition-all flex flex-col items-center">
                                    <div className="h-40 w-40 mb-4 flex items-center justify-center">
                                        <ConstellationRenderer stars={c.stars} lines={c.lines} size={150} animated={true} />
                                    </div>
                                    <h3 className="text-xl font-bold text-yellow-400 mb-2">{c.name}</h3>
                                    <p className="text-sm text-gray-400 text-center">{c.desc}</p>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            </div>
        )
    }

    if (!current) return <div>Loading Sky...</div>;

    return (
        <div className="max-w-xl mx-auto animate-fade-in">
             <div className="flex justify-between items-center mb-4 text-sm text-gray-500">
                <span>Constellations remaining: {remaining}</span>
                <button onClick={() => setViewMode('MAP')} className="flex items-center gap-2 text-yellow-400 hover:text-yellow-300 font-bold">
                    <Map size={16}/> View Star Map
                </button>
            </div>
            
            <Card className="flex flex-col items-center justify-center min-h-[400px] relative overflow-hidden bg-gray-950 border-gray-800">
                {/* Background Stars for Atmosphere */}
                <div className="absolute inset-0 pointer-events-none opacity-50">
                    {[...Array(20)].map((_, i) => (
                        <div key={i} className="absolute bg-white rounded-full w-0.5 h-0.5" style={{ top: `${Math.random()*100}%`, left: `${Math.random()*100}%` }}/>
                    ))}
                </div>

                <div className="z-10 mb-8 p-8 bg-gradient-to-b from-gray-900 to-transparent rounded-full border border-gray-800 shadow-2xl">
                    <ConstellationRenderer stars={current.stars} lines={current.lines} size={250} animated={true} />
                </div>

                {!reveal ? (
                    <div className="z-10 text-center w-full">
                        <p className="text-gray-400 mb-4">Identify this constellation</p>
                        <Button onClick={() => setReveal(true)} variant="primary" className="mx-auto w-full max-w-xs">Reveal Answer</Button>
                    </div>
                ) : (
                    <div className="z-10 text-center animate-fade-in w-full">
                        <h2 className="text-3xl font-bold text-yellow-400 mb-2">{current.name}</h2>
                        <p className="text-gray-300 mb-6 max-w-sm mx-auto">{current.desc}</p>
                        <Button onClick={() => { setReveal(false); draw(); }} variant="outline" className="mx-auto">Next Constellation</Button>
                    </div>
                )}
            </Card>
        </div>
    );
};

// --- NEW TLD GAME ---
const TLDGame = () => {
    const { current, draw, remaining } = useDeck(TLD_DATA);
    const [options, setOptions] = useState([]);
    const [score, setScore] = useState(0);
    const [feedback, setFeedback] = useState(null); // 'correct' | 'wrong'

    useEffect(() => {
        if (!current) return;
        const distractors = TLD_DATA.filter(t => t.tld !== current.tld)
            .sort(() => 0.5 - Math.random())
            .slice(0, 3);
        setOptions([current, ...distractors].sort(() => 0.5 - Math.random()));
        setFeedback(null);
    }, [current]);

    const handleAnswer = (opt) => {
        if (feedback) return;
        if (opt.tld === current.tld) {
            setScore(s => s + 1);
            setFeedback('correct');
        } else {
            setScore(0);
            setFeedback('wrong');
        }
    };

    if (!current) return <div>Loading...</div>;

    return (
        <div className="max-w-xl mx-auto animate-fade-in">
            <div className="text-center text-xs text-gray-500 mb-4">Domains remaining: {remaining}</div>
            
            <Card className="text-center mb-8 bg-gradient-to-br from-blue-900/50 to-gray-900 border-blue-800/50">
                <Wifi className="mx-auto text-blue-400 mb-4" size={40}/>
                <h3 className="text-gray-400 text-sm uppercase tracking-widest mb-2">Which domain belongs to</h3>
                <h2 className="text-4xl font-bold text-white mb-2">{current.country}?</h2>
                <div className="text-xs text-blue-400 font-bold tracking-widest mt-4">STREAK: {score}</div>
            </Card>

            {feedback ? (
                <div className="animate-fade-in text-center">
                    <div className={`p-6 rounded-xl border mb-6 ${feedback === 'correct' ? 'bg-green-900/30 border-green-500/50' : 'bg-red-900/30 border-red-500/50'}`}>
                        <h3 className={`text-2xl font-bold mb-2 ${feedback === 'correct' ? 'text-green-400' : 'text-red-400'}`}>
                            {feedback === 'correct' ? 'Correct!' : 'Incorrect!'}
                        </h3>
                        <p className="text-white text-lg font-mono mb-4">The domain is <span className="text-blue-400 font-bold">{current.tld}</span></p>
                        <div className="bg-black/30 p-4 rounded-lg text-gray-300 text-sm italic border border-white/10">
                            "{current.fact}"
                        </div>
                    </div>
                    <Button onClick={draw} variant="primary" className="mx-auto w-full max-w-xs">Next Domain <ArrowRight size={16}/></Button>
                </div>
            ) : (
                <div className="grid grid-cols-2 gap-4">
                    {options.map((opt, i) => (
                        <button 
                            key={i} 
                            onClick={() => handleAnswer(opt)}
                            className="p-6 rounded-xl text-2xl font-mono font-bold transition-all border-2 bg-gray-800 border-gray-700 hover:bg-gray-700 hover:border-blue-500 hover:text-blue-400"
                        >
                            {opt.tld}
                        </button>
                    ))}
                </div>
            )}
        </div>
    );
};

// --- PRESIDENTS GAME ---
const PresidentsGame = () => {
    const { current, draw, remaining } = useDeck(US_PRESIDENTS);
    const [options, setOptions] = useState([]);
    const [score, setScore] = useState(0);
    const [feedback, setFeedback] = useState(null);

    useEffect(() => {
        if (!current) return;
        const distractors = US_PRESIDENTS.filter(p => p.n !== current.n).sort(() => 0.5 - Math.random()).slice(0, 3);
        setOptions([current, ...distractors].sort(() => 0.5 - Math.random()));
        setFeedback(null);
    }, [current]);

    const handleAnswer = (opt) => {
        if (feedback) return;
        if (opt.n === current.n) {
            setScore(s => s + 1);
            setFeedback('correct');
        } else {
            setScore(0);
            setFeedback('wrong');
        }
    };

    if (!current) return <div>Loading...</div>;

    // Helper to add ordinal suffix
    const getOrdinal = (n) => {
        const s = ["th", "st", "nd", "rd"];
        const v = n % 100;
        return n + (s[(v - 20) % 10] || s[v] || s[0]);
    };

    return (
        <div className="max-w-xl mx-auto animate-fade-in">
             <div className="text-center text-xs text-gray-500 mb-4">Presidents remaining: {remaining}</div>
            <Card className="text-center mb-8 bg-gradient-to-br from-blue-900/50 to-red-900/50 border-white/10">
                <Crown className="mx-auto text-yellow-400 mb-4" size={40}/>
                <h3 className="text-gray-400 text-sm uppercase tracking-widest mb-2">Who was the</h3>
                <h2 className="text-4xl font-bold text-white mb-2">{getOrdinal(current.n)} President?</h2>
                <div className="text-xs text-blue-400 font-bold tracking-widest mt-4">STREAK: {score}</div>
            </Card>

            {feedback ? (
                <div className="animate-fade-in text-center">
                    <div className={`p-6 rounded-xl border mb-6 ${feedback === 'correct' ? 'bg-green-900/30 border-green-500/50' : 'bg-red-900/30 border-red-500/50'}`}>
                        <h3 className={`text-2xl font-bold mb-2 ${feedback === 'correct' ? 'text-green-400' : 'text-red-400'}`}>
                            {feedback === 'correct' ? 'Correct!' : 'Incorrect!'}
                        </h3>
                        <p className="text-white text-lg mb-2">It was <span className="text-yellow-400 font-bold">{current.name}</span></p>
                    </div>
                    <Button onClick={draw} variant="primary" className="mx-auto w-full max-w-xs">Next President <ArrowRight size={16}/></Button>
                </div>
            ) : (
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                    {options.map((opt, i) => (
                        <button 
                            key={i} 
                            onClick={() => handleAnswer(opt)}
                            className="p-4 rounded-xl text-lg font-bold transition-all border-2 bg-gray-800 border-gray-700 hover:bg-gray-700 hover:border-blue-500 hover:text-blue-400"
                        >
                            {opt.name}
                        </button>
                    ))}
                </div>
            )}
        </div>
    );
};

// --- MONARCHS GAME ---
const MonarchsGame = () => {
    // We only quiz up to Elizabeth II, as Charles III is the last one
    const quizList = BRITISH_MONARCHS.slice(0, BRITISH_MONARCHS.length - 1);
    const { current, draw, remaining } = useDeck(quizList);
    
    const [options, setOptions] = useState([]);
    const [score, setScore] = useState(0);
    const [feedback, setFeedback] = useState(null);
    const [showList, setShowList] = useState(false);

    useEffect(() => {
        if (!current) return;
        const currentIndex = BRITISH_MONARCHS.indexOf(current);
        const correctSuccessor = BRITISH_MONARCHS[currentIndex + 1];
        
        // Distractors: pick random monarchs that aren't the correct successor
        const potentialDistractors = BRITISH_MONARCHS.filter(m => m !== correctSuccessor && m !== current);
        const distractors = potentialDistractors.sort(() => 0.5 - Math.random()).slice(0, 3);
        
        setOptions([correctSuccessor, ...distractors].sort(() => 0.5 - Math.random()));
        setFeedback(null);
    }, [current]);

    const handleAnswer = (opt) => {
        if (feedback) return;
        const currentIndex = BRITISH_MONARCHS.indexOf(current);
        const correctSuccessor = BRITISH_MONARCHS[currentIndex + 1];

        if (opt === correctSuccessor) {
            setScore(s => s + 1);
            setFeedback('correct');
        } else {
            setScore(0);
            setFeedback('wrong');
        }
    };

    if (!current) return <div>Loading...</div>;
    
    const currentIndex = BRITISH_MONARCHS.indexOf(current);
    const correctSuccessor = BRITISH_MONARCHS[currentIndex + 1];

    if (showList) {
        return (
            <div className="fixed inset-0 bg-gray-950 z-[100] overflow-hidden flex flex-col animate-fade-in">
                <div className="absolute inset-0 overflow-auto p-8">
                     <div className="max-w-3xl mx-auto">
                        <div className="flex justify-between items-center mb-8 sticky top-0 bg-gray-950/90 backdrop-blur p-4 border-b border-gray-800 z-10">
                             <h2 className="text-3xl font-bold text-yellow-100">Royal Lineage</h2>
                             <Button onClick={() => setShowList(false)} variant="secondary">Close List</Button>
                        </div>
                        <div className="space-y-2">
                            {BRITISH_MONARCHS.map((m, i) => (
                                <div key={i} className="flex items-center p-4 bg-gray-900 rounded-lg border border-gray-800">
                                    <div className="w-12 h-12 bg-gray-800 rounded-full flex items-center justify-center font-bold text-gray-500 mr-4">
                                        {i + 1}
                                    </div>
                                    <div className="text-xl font-serif text-gray-200">{m}</div>
                                    {i < BRITISH_MONARCHS.length - 1 && (
                                        <div className="ml-auto text-gray-600">↓</div>
                                    )}
                                </div>
                            ))}
                        </div>
                     </div>
                </div>
            </div>
        )
    }

    return (
        <div className="max-w-xl mx-auto animate-fade-in">
             <div className="flex justify-between items-center mb-4 text-sm text-gray-500">
                <span>Monarchs remaining: {remaining}</span>
                <button onClick={() => setShowList(true)} className="flex items-center gap-2 text-yellow-400 hover:text-yellow-300 font-bold">
                    <ScrollText size={16}/> View Lineage
                </button>
            </div>
            
            <Card className="text-center mb-8 bg-gradient-to-br from-purple-900/50 to-gray-900 border-purple-800/50">
                <Crown className="mx-auto text-yellow-400 mb-4" size={40}/>
                <h3 className="text-gray-400 text-sm uppercase tracking-widest mb-2">Who succeeded</h3>
                <h2 className="text-4xl font-bold font-serif text-white mb-2">{current}?</h2>
                <div className="text-xs text-purple-400 font-bold tracking-widest mt-4">STREAK: {score}</div>
            </Card>

            {feedback ? (
                <div className="animate-fade-in text-center">
                    <div className={`p-6 rounded-xl border mb-6 ${feedback === 'correct' ? 'bg-green-900/30 border-green-500/50' : 'bg-red-900/30 border-red-500/50'}`}>
                        <h3 className={`text-2xl font-bold mb-2 ${feedback === 'correct' ? 'text-green-400' : 'text-red-400'}`}>
                            {feedback === 'correct' ? 'Correct!' : 'Incorrect!'}
                        </h3>
                        <p className="text-white text-lg mb-2">The successor was <span className="text-yellow-400 font-serif font-bold">{correctSuccessor}</span></p>
                    </div>
                    <Button onClick={draw} variant="primary" className="mx-auto w-full max-w-xs">Next Monarch <ArrowRight size={16}/></Button>
                </div>
            ) : (
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                    {options.map((opt, i) => (
                        <button 
                            key={i} 
                            onClick={() => handleAnswer(opt)}
                            className="p-4 rounded-xl text-lg font-serif font-bold transition-all border-2 bg-gray-800 border-gray-700 hover:bg-gray-700 hover:border-purple-500 hover:text-purple-400"
                        >
                            {opt}
                        </button>
                    ))}
                </div>
            )}
        </div>
    );
};

// --- NATO GAME ---
const NatoGame = () => {
    const { current, draw, remaining } = useDeck(NATO_PHONETIC);
    const [input, setInput] = useState("");
    const [feedback, setFeedback] = useState(null); // 'correct', 'wrong'
    const [score, setScore] = useState(0);

    const checkAnswer = () => {
        if (feedback) return;
        const fullWord = current.w.toLowerCase();
        const guess = (current.l + input).toLowerCase();
        
        const cleanTarget = fullWord.replace(/[^a-z]/g, '');
        const cleanGuess = guess.replace(/[^a-z]/g, '');

        if (cleanGuess === cleanTarget) {
            setScore(s => s + 1);
            setFeedback('correct');
            setTimeout(() => {
                draw();
                setInput("");
                setFeedback(null);
            }, 1000);
        } else {
            setScore(0);
            setFeedback('wrong');
            setTimeout(() => {
                setInput("");
                setFeedback(null);
            }, 1000);
        }
    };

    if (!current) return <div>Loading...</div>;

    return (
        <div className="max-w-xl mx-auto animate-fade-in">
            <div className="text-center text-xs text-gray-500 mb-4">Letters remaining: {remaining}</div>
            <div className="flex justify-center mb-8">
                <div className="text-xs text-green-400 font-bold tracking-widest bg-gray-800 px-4 py-1 rounded-full border border-gray-700">STREAK: {score}</div>
            </div>

            <Card className={`text-center py-16 transition-colors duration-300 ${feedback === 'correct' ? 'border-green-500 bg-green-900/20' : feedback === 'wrong' ? 'border-red-500 bg-red-900/20' : 'border-gray-700'}`}>
                <div className="flex items-center justify-center gap-1 text-5xl md:text-7xl font-mono font-bold text-white">
                    <span className="text-blue-400">{current.l}</span>
                    <input 
                        autoFocus
                        type="text" 
                        value={input}
                        onChange={(e) => setInput(e.target.value)}
                        onKeyDown={(e) => e.key === 'Enter' && checkAnswer()}
                        className="bg-transparent outline-none text-gray-400 placeholder-gray-700 w-full max-w-[300px]"
                        placeholder={current.w.slice(1).replace(/./g, '_')}
                        spellCheck="false"
                    />
                </div>
                <p className="text-gray-500 mt-4 text-sm">Complete the NATO phonetic word</p>
            </Card>
            
            <div className="mt-8 flex justify-center">
                 <Button onClick={checkAnswer} variant="primary" className="px-8 py-3 text-lg">Submit</Button>
            </div>
            
            {feedback === 'wrong' && (
                <div className="text-center mt-4 text-red-400 font-mono font-bold animate-bounce">
                    {current.w}
                </div>
            )}
        </div>
    );
};

// --- MORSE GAME ---
const MorseGame = () => {
    const { current, draw, remaining } = useDeck(MORSE_CODE);
    const [options, setOptions] = useState([]);
    const [score, setScore] = useState(0);
    const [feedback, setFeedback] = useState(null);
    const [showCheatSheet, setShowCheatSheet] = useState(false);

    useEffect(() => {
        if (!current) return;
        // Distractors logic
        const distractors = MORSE_CODE.filter(i => i.l !== current.l).sort(() => 0.5 - Math.random()).slice(0, 3);
        setOptions([current, ...distractors].sort(() => 0.5 - Math.random()));
        setFeedback(null);
    }, [current]);

    const handleAnswer = (opt) => {
        if (feedback) return;
        if (opt.l === current.l) {
            setScore(s => s + 1);
            setFeedback('correct');
            setTimeout(draw, 800);
        } else {
            setScore(0);
            setFeedback('wrong');
            setTimeout(draw, 1500);
        }
    };

    if (!current) return <div>Loading...</div>;

    if (showCheatSheet) {
        return (
            <div className="fixed inset-0 bg-gray-950 z-[100] overflow-hidden flex flex-col animate-fade-in">
                <div className="absolute inset-0 overflow-auto p-8">
                     <div className="max-w-4xl mx-auto">
                        <div className="flex justify-between items-center mb-8 sticky top-0 bg-gray-950/90 backdrop-blur p-4 border-b border-gray-800 z-10">
                             <h2 className="text-3xl font-bold text-blue-100">Morse Code Alphabet</h2>
                             <Button onClick={() => setShowCheatSheet(false)} variant="secondary">Close</Button>
                        </div>
                        <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                            {MORSE_CODE.map((m, i) => (
                                <div key={i} className="flex justify-between items-center p-4 bg-gray-900 rounded-lg border border-gray-800">
                                    <span className="text-2xl font-bold text-white">{m.l}</span>
                                    <span className="text-xl font-mono text-blue-400 tracking-widest">{m.c}</span>
                                </div>
                            ))}
                        </div>
                     </div>
                </div>
            </div>
        )
    }

    return (
        <div className="max-w-xl mx-auto animate-fade-in">
             <div className="flex justify-between items-center mb-4 text-sm text-gray-500">
                <span>Symbols remaining: {remaining}</span>
                <button onClick={() => setShowCheatSheet(true)} className="flex items-center gap-2 text-blue-400 hover:text-blue-300 font-bold">
                    <Search size={16}/> View Alphabet
                </button>
            </div>
            
            <Card className="text-center py-16 mb-8 border-blue-500/30 bg-blue-900/10">
                <h2 className="text-6xl font-mono font-bold text-blue-400 tracking-widest mb-2">{current.c}</h2>
                <div className="text-xs text-blue-500 font-bold tracking-widest mt-8 uppercase">Decode this symbol</div>
                <div className="text-xs text-gray-500 font-bold tracking-widest mt-2">STREAK: {score}</div>
            </Card>

            <div className="grid grid-cols-2 gap-4">
                {options.map((opt, i) => (
                    <button 
                        key={i} 
                        onClick={() => handleAnswer(opt)}
                        className={`p-6 rounded-xl text-3xl font-bold transition-all border-2 
                            ${feedback === 'correct' && opt.l === current.l ? 'bg-green-600 border-green-400 text-white' : 
                              feedback === 'wrong' && opt.l === current.l ? 'bg-green-600 border-green-400 text-white' :
                              feedback === 'wrong' && opt.l !== current.l ? 'opacity-30' : 
                              'bg-gray-800 border-gray-600 hover:bg-gray-700'}`}
                    >
                        {opt.l}
                    </button>
                ))}
            </div>
        </div>
    );
};

// --- BRAILLE GAME ---
const BrailleGame = () => {
    const { current, draw, remaining } = useDeck(BRAILLE_CODE);
    const [options, setOptions] = useState([]);
    const [score, setScore] = useState(0);
    const [feedback, setFeedback] = useState(null);
    const [showCheatSheet, setShowCheatSheet] = useState(false);

    useEffect(() => {
        if (!current) return;
        const distractors = BRAILLE_CODE.filter(i => i.l !== current.l).sort(() => 0.5 - Math.random()).slice(0, 3);
        setOptions([current, ...distractors].sort(() => 0.5 - Math.random()));
        setFeedback(null);
    }, [current]);

    const handleAnswer = (opt) => {
        if (feedback) return;
        if (opt.l === current.l) {
            setScore(s => s + 1);
            setFeedback('correct');
            setTimeout(draw, 800);
        } else {
            setScore(0);
            setFeedback('wrong');
            setTimeout(draw, 1500);
        }
    };

    if (!current) return <div>Loading...</div>;

    if (showCheatSheet) {
        return (
            <div className="fixed inset-0 bg-gray-950 z-[100] overflow-hidden flex flex-col animate-fade-in">
                <div className="absolute inset-0 overflow-auto p-8">
                     <div className="max-w-4xl mx-auto">
                        <div className="flex justify-between items-center mb-8 sticky top-0 bg-gray-950/90 backdrop-blur p-4 border-b border-gray-800 z-10">
                             <h2 className="text-3xl font-bold text-pink-100">Braille Alphabet</h2>
                             <Button onClick={() => setShowCheatSheet(false)} variant="secondary">Close</Button>
                        </div>
                        <div className="grid grid-cols-3 md:grid-cols-6 gap-4">
                            {BRAILLE_CODE.map((m, i) => (
                                <div key={i} className="flex flex-col items-center p-4 bg-gray-900 rounded-lg border border-gray-800 hover:border-pink-500 transition-colors">
                                    <span className="text-4xl text-pink-400 mb-2">{m.c}</span>
                                    <span className="text-xl font-bold text-white">{m.l}</span>
                                </div>
                            ))}
                        </div>
                     </div>
                </div>
            </div>
        )
    }

    return (
        <div className="max-w-xl mx-auto animate-fade-in">
             <div className="flex justify-between items-center mb-4 text-sm text-gray-500">
                <span>Symbols remaining: {remaining}</span>
                <button onClick={() => setShowCheatSheet(true)} className="flex items-center gap-2 text-pink-400 hover:text-pink-300 font-bold">
                    <Glasses size={16}/> View Braille
                </button>
            </div>
            
            <Card className="text-center py-16 mb-8 border-pink-500/30 bg-pink-900/10">
                <h2 className="text-8xl font-bold text-pink-400 mb-2">{current.c}</h2>
                <div className="text-xs text-pink-500 font-bold tracking-widest mt-8 uppercase">Decode this symbol</div>
                <div className="text-xs text-gray-500 font-bold tracking-widest mt-2">STREAK: {score}</div>
            </Card>

            <div className="grid grid-cols-2 gap-4">
                {options.map((opt, i) => (
                    <button 
                        key={i} 
                        onClick={() => handleAnswer(opt)}
                        className={`p-6 rounded-xl text-3xl font-bold transition-all border-2 
                            ${feedback === 'correct' && opt.l === current.l ? 'bg-green-600 border-green-400 text-white' : 
                              feedback === 'wrong' && opt.l === current.l ? 'bg-green-600 border-green-400 text-white' :
                              feedback === 'wrong' && opt.l !== current.l ? 'opacity-30' : 
                              'bg-gray-800 border-gray-600 hover:bg-gray-700'}`}
                    >
                        {opt.l}
                    </button>
                ))}
            </div>
        </div>
    );
};

// --- MAIN APP ---

export default function App() {
    const [view, setView] = useState("HOME");

    const renderGame = () => {
        switch (view) {
            case "PI": return <PiGame />;
            case "ELEMENTS": return <PeriodicGame />;
            case "FLAGS": return <FlagGame />;
            case "GEO": return <GeoGame />;
            case "LANG": return <LanguageGame />;
            case "ALLIANCE": return <AllianceGame />;
            case "EMPIRES": return <EmpireGame />;
            case "DISH": return <DishGame />;
            case "POP": return <PopulationGame />;
            case "CODE": return <CodeGame />;
            case "STARS": return <ConstellationGame />;
            case "TLD": return <TLDGame />;
            case "PRESIDENTS": return <PresidentsGame />;
            case "MONARCHS": return <MonarchsGame />;
            case "NATO": return <NatoGame />;
            case "MORSE": return <MorseGame />;
            case "BRAILLE": return <BrailleGame />;
            default: return null;
        }
    };

    return (
        <div className="min-h-screen flex flex-col bg-gray-900 text-white font-sans">
             <style>{`
                @import url('https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;700&family=Inter:wght@400;600;800&family=Cinzel:wght@400;700&display=swap');
                .font-mono { font-family: 'JetBrains Mono', monospace; }
                .font-sans { font-family: 'Inter', sans-serif; }
                .font-serif { font-family: 'Cinzel', serif; }
                .animate-wiggle { animation: wiggle 0.3s ease-in-out; }
                @keyframes wiggle { 0%, 100% { transform: rotate(0deg); } 25% { transform: rotate(-5deg); } 75% { transform: rotate(5deg); } }
                .animate-fade-in { animation: fadeIn 0.5s ease-out; }
                @keyframes fadeIn { from { opacity: 0; transform: translateY(10px); } to { opacity: 1; transform: translateY(0); } }
                
                @keyframes twinkle { 
                    0%, 100% { opacity: 1; transform: scale(1); } 
                    50% { opacity: 0.3; transform: scale(0.8); } 
                }
                .animate-twinkle { animation: twinkle 3s infinite ease-in-out; }
                .drop-shadow-glow { filter: drop-shadow(0 0 4px rgba(255, 255, 255, 0.8)); }

                ::-webkit-scrollbar { width: 8px; }
                ::-webkit-scrollbar-track { background: #1f2937; }
                ::-webkit-scrollbar-thumb { background: #4b5563; border-radius: 4px; }
                ::-webkit-scrollbar-thumb:hover { background: #6b7280; }
                .perspective-1000 { perspective: 1000px; }
                .transform-style-3d { transform-style: preserve-3d; }
                .backface-hidden { backface-visibility: hidden; }
            `}</style>
            
            <header className="bg-gray-900/50 backdrop-blur border-b border-gray-800 sticky top-0 z-50">
                <div className="max-w-5xl mx-auto px-4 h-16 flex items-center justify-between">
                    <div className="flex items-center gap-3 cursor-pointer" onClick={() => setView("HOME")}>
                        <Brain className="text-purple-500" size={24} />
                        <h1 className="font-bold text-lg md:text-xl tracking-tight">
                            Useless<span className="text-purple-500">Info</span>Trainer
                        </h1>
                    </div>
                    {view !== "HOME" && (
                        <button onClick={() => setView("HOME")} className="flex items-center gap-1 text-sm text-gray-400 hover:text-white transition-colors">
                            <ChevronLeft size={16} /> Back to Menu
                        </button>
                    )}
                </div>
            </header>

            <main className="flex-1 p-4 md:p-8 overflow-y-auto">
                {view === "HOME" ? (
                    <div className="max-w-5xl mx-auto animate-fade-in">
                        <div className="text-center mb-12">
                            <h2 className="text-3xl md:text-5xl font-bold mb-4 bg-clip-text text-transparent bg-gradient-to-r from-purple-400 to-pink-400">low taper fade</h2>
                            <p className="text-gray-400 text-lg">select a module to start</p>
                        </div>

                        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4 md:gap-6">
                            <Card onClick={() => setView("PI")} className="group hover:border-purple-500 cursor-pointer bg-gradient-to-br from-gray-800 to-gray-800/50">
                                <div className="flex justify-between items-start mb-4">
                                    <div className="p-3 bg-purple-500/20 rounded-lg text-purple-400 group-hover:bg-purple-500 group-hover:text-white transition-colors"><span className="font-mono font-bold text-xl">π</span></div>
                                    <Info size={16} className="text-gray-600" />
                                </div>
                                <h3 className="text-xl font-bold mb-2">100 Digits of Pi</h3>
                                <p className="text-sm text-gray-400">Memorize the mathematical constant.</p>
                            </Card>

                            <Card onClick={() => setView("PRESIDENTS")} className="group hover:border-red-500 cursor-pointer bg-gradient-to-br from-gray-800 to-gray-800/50">
                                <div className="flex justify-between items-start mb-4">
                                    <div className="p-3 bg-red-500/20 rounded-lg text-red-400 group-hover:bg-red-500 group-hover:text-white transition-colors"><Flag size={24} /></div>
                                    <span className="text-xs font-bold bg-red-500 text-white px-2 py-0.5 rounded">NEW</span>
                                </div>
                                <h3 className="text-xl font-bold mb-2">US Presidents</h3>
                                <p className="text-sm text-gray-400">Match the President to their number in history.</p>
                            </Card>

                             <Card onClick={() => setView("MONARCHS")} className="group hover:border-purple-300 cursor-pointer bg-gradient-to-br from-gray-800 to-gray-800/50">
                                <div className="flex justify-between items-start mb-4">
                                    <div className="p-3 bg-purple-500/20 rounded-lg text-purple-400 group-hover:bg-purple-500 group-hover:text-white transition-colors"><Crown size={24} /></div>
                                    <span className="text-xs font-bold bg-purple-500 text-white px-2 py-0.5 rounded">NEW</span>
                                </div>
                                <h3 className="text-xl font-bold mb-2">Royal Lineage</h3>
                                <p className="text-sm text-gray-400">Succession from William the Conqueror.</p>
                            </Card>

                            <Card onClick={() => setView("ALLIANCE")} className="group hover:border-cyan-500 cursor-pointer bg-gradient-to-br from-gray-800 to-gray-800/50">
                                <div className="flex justify-between items-start mb-4">
                                    <div className="p-3 bg-cyan-500/20 rounded-lg text-cyan-400 group-hover:bg-cyan-500 group-hover:text-white transition-colors"><Shield size={24} /></div>
                                    <span className="text-xs font-bold bg-cyan-500 text-white px-2 py-0.5 rounded">NEW</span>
                                </div>
                                <h3 className="text-xl font-bold mb-2">Alliance Guesser</h3>
                                <p className="text-sm text-gray-400">Name all member states of NATO, EU, and more.</p>
                            </Card>

                            <Card onClick={() => setView("EMPIRES")} className="group hover:border-yellow-500 cursor-pointer bg-gradient-to-br from-gray-800 to-gray-800/50">
                                <div className="flex justify-between items-start mb-4">
                                    <div className="p-3 bg-yellow-500/20 rounded-lg text-yellow-400 group-hover:bg-yellow-500 group-hover:text-white transition-colors"><Castle size={24} /></div>
                                    <span className="text-xs font-bold bg-yellow-500 text-white px-2 py-0.5 rounded">NEW</span>
                                </div>
                                <h3 className="text-xl font-bold mb-2">Empire Builder</h3>
                                <p className="text-sm text-gray-400">Name modern territories of historical empires.</p>
                            </Card>

                            <Card onClick={() => setView("STARS")} className="group hover:border-yellow-300 cursor-pointer bg-gradient-to-br from-gray-800 to-gray-800/50">
                                <div className="flex justify-between items-start mb-4">
                                    <div className="p-3 bg-yellow-500/20 rounded-lg text-yellow-400 group-hover:bg-yellow-500 group-hover:text-white transition-colors"><Star size={24} /></div>
                                    <span className="text-xs font-bold bg-yellow-500 text-white px-2 py-0.5 rounded">NEW</span>
                                </div>
                                <h3 className="text-xl font-bold mb-2">Constellations</h3>
                                <p className="text-sm text-gray-400">Identify patterns in the night sky.</p>
                            </Card>

                            <Card onClick={() => setView("TLD")} className="group hover:border-blue-300 cursor-pointer bg-gradient-to-br from-gray-800 to-gray-800/50">
                                <div className="flex justify-between items-start mb-4">
                                    <div className="p-3 bg-blue-500/20 rounded-lg text-blue-400 group-hover:bg-blue-500 group-hover:text-white transition-colors"><Wifi size={24} /></div>
                                    <span className="text-xs font-bold bg-blue-500 text-white px-2 py-0.5 rounded">NEW</span>
                                </div>
                                <h3 className="text-xl font-bold mb-2">Domain Namer</h3>
                                <p className="text-sm text-gray-400">Guess the Top-Level Domain and learn facts.</p>
                            </Card>

                            <Card onClick={() => setView("CODE")} className="group hover:border-emerald-500 cursor-pointer bg-gradient-to-br from-gray-800 to-gray-800/50">
                                <div className="flex justify-between items-start mb-4">
                                    <div className="p-3 bg-emerald-500/20 rounded-lg text-emerald-400 group-hover:bg-emerald-500 group-hover:text-white transition-colors"><Terminal size={24} /></div>
                                    <span className="text-xs font-bold bg-emerald-500 text-white px-2 py-0.5 rounded">NEW</span>
                                </div>
                                <h3 className="text-xl font-bold mb-2">Hello World</h3>
                                <p className="text-sm text-gray-400">Code "Hello, World!" in 30+ languages.</p>
                            </Card>

                            <Card onClick={() => setView("NATO")} className="group hover:border-slate-500 cursor-pointer bg-gradient-to-br from-gray-800 to-gray-800/50">
                                <div className="flex justify-between items-start mb-4">
                                    <div className="p-3 bg-slate-500/20 rounded-lg text-slate-400 group-hover:bg-slate-500 group-hover:text-white transition-colors"><Radio size={24} /></div>
                                    <span className="text-xs font-bold bg-slate-500 text-white px-2 py-0.5 rounded">NEW</span>
                                </div>
                                <h3 className="text-xl font-bold mb-2">NATO Phonetic</h3>
                                <p className="text-sm text-gray-400">Complete the words: Alpha, Bravo, Charlie...</p>
                            </Card>

                            <Card onClick={() => setView("MORSE")} className="group hover:border-blue-600 cursor-pointer bg-gradient-to-br from-gray-800 to-gray-800/50">
                                <div className="flex justify-between items-start mb-4">
                                    <div className="p-3 bg-blue-600/20 rounded-lg text-blue-400 group-hover:bg-blue-600 group-hover:text-white transition-colors"><Search size={24} /></div>
                                    <span className="text-xs font-bold bg-blue-600 text-white px-2 py-0.5 rounded">NEW</span>
                                </div>
                                <h3 className="text-xl font-bold mb-2">Morse Code</h3>
                                <p className="text-sm text-gray-400">Decode the dots and dashes.</p>
                            </Card>

                            <Card onClick={() => setView("BRAILLE")} className="group hover:border-pink-600 cursor-pointer bg-gradient-to-br from-gray-800 to-gray-800/50">
                                <div className="flex justify-between items-start mb-4">
                                    <div className="p-3 bg-pink-600/20 rounded-lg text-pink-400 group-hover:bg-pink-600 group-hover:text-white transition-colors"><Glasses size={24} /></div>
                                    <span className="text-xs font-bold bg-pink-600 text-white px-2 py-0.5 rounded">NEW</span>
                                </div>
                                <h3 className="text-xl font-bold mb-2">Braille</h3>
                                <p className="text-sm text-gray-400">Read by sight what others feel.</p>
                            </Card>

                            <Card onClick={() => setView("FLAGS")} className="group hover:border-yellow-500 cursor-pointer bg-gradient-to-br from-gray-800 to-gray-800/50">
                                <div className="flex justify-between items-start mb-4">
                                    <div className="p-3 bg-yellow-500/20 rounded-lg text-yellow-400 group-hover:bg-yellow-500 group-hover:text-white transition-colors"><Flag size={24} /></div>
                                </div>
                                <h3 className="text-xl font-bold mb-2">Vexillology</h3>
                                <p className="text-sm text-gray-400">Identify flags from every continent.</p>
                            </Card>

                            <Card onClick={() => setView("ELEMENTS")} className="group hover:border-green-500 cursor-pointer bg-gradient-to-br from-gray-800 to-gray-800/50">
                                <div className="flex justify-between items-start mb-4">
                                    <div className="p-3 bg-green-500/20 rounded-lg text-green-400 group-hover:bg-green-500 group-hover:text-white transition-colors"><FlaskConical size={24} /></div>
                                </div>
                                <h3 className="text-xl font-bold mb-2">Periodic Table</h3>
                                <p className="text-sm text-gray-400">Match symbols to names.</p>
                            </Card>

                            <Card onClick={() => setView("GEO")} className="group hover:border-blue-500 cursor-pointer bg-gradient-to-br from-gray-800 to-gray-800/50">
                                <div className="flex justify-between items-start mb-4">
                                    <div className="p-3 bg-blue-500/20 rounded-lg text-blue-400 group-hover:bg-blue-500 group-hover:text-white transition-colors"><Globe size={24} /></div>
                                </div>
                                <h3 className="text-xl font-bold mb-2">Capital Cities</h3>
                                <p className="text-sm text-gray-400">Guess the capital cities of countries around the world.</p>
                            </Card>

                            <Card onClick={() => setView("LANG")} className="group hover:border-pink-500 cursor-pointer bg-gradient-to-br from-gray-800 to-gray-800/50">
                                <div className="flex justify-between items-start mb-4">
                                    <div className="p-3 bg-pink-500/20 rounded-lg text-pink-400 group-hover:bg-pink-500 group-hover:text-white transition-colors"><Languages size={24} /></div>
                                </div>
                                <h3 className="text-xl font-bold mb-2">Hello Person</h3>
                                <p className="text-sm text-gray-400">Learn to say "Hello" in 30+ languages.</p>
                            </Card>

                            <Card onClick={() => setView("DISH")} className="group hover:border-orange-500 cursor-pointer bg-gradient-to-br from-gray-800 to-gray-800/50">
                                <div className="flex justify-between items-start mb-4">
                                    <div className="p-3 bg-orange-500/20 rounded-lg text-orange-400 group-hover:bg-orange-500 group-hover:text-white transition-colors"><Utensils size={24} /></div>
                                    <span className="text-xs font-bold bg-orange-500 text-white px-2 py-0.5 rounded">NEW</span>
                                </div>
                                <h3 className="text-xl font-bold mb-2">National Dish</h3>
                                <p className="text-sm text-gray-400">Match the country to its famous cuisine.</p>
                            </Card>

                            <Card onClick={() => setView("POP")} className="group hover:border-indigo-500 cursor-pointer bg-gradient-to-br from-gray-800 to-gray-800/50">
                                <div className="flex justify-between items-start mb-4">
                                    <div className="p-3 bg-indigo-500/20 rounded-lg text-indigo-400 group-hover:bg-indigo-500 group-hover:text-white transition-colors"><Users size={24} /></div>
                                    <span className="text-xs font-bold bg-indigo-500 text-white px-2 py-0.5 rounded">NEW</span>
                                </div>
                                <h3 className="text-xl font-bold mb-2">Population Master</h3>
                                <p className="text-sm text-gray-400">Guess populations to the nearest million.</p>
                            </Card>

                        </div>
                    </div>
                ) : (
                    renderGame()
                )}
            </main>
        </div>
    );
}
