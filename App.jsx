import React, { useState, useEffect, useMemo, useRef } from 'react';
import { Brain, Globe, FlaskConical, Flag, Languages, ChevronLeft, Info, Check, X, Utensils, Users, Shield, ArrowRight, Terminal, Lock, Unlock, Star, Map, Wifi, Crown, ScrollText, Castle, Radio, Search, Glasses, Cpu, Phone, Coins, Hash, Trophy, Eye, EyeOff, Keyboard, RotateCcw, Music, CalendarDays } from 'lucide-react';

// --- DATASETS ---

const PI_100 = "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679";

const ELEMENTS = [
    { n: 1, s: 'H', name: 'Hydrogen' }, { n: 2, s: 'He', name: 'Helium' }, { n: 3, s: 'Li', name: 'Lithium' }, 
    { n: 4, s: 'Be', name: 'Beryllium' }, { n: 5, s: 'B', name: 'Boron' }, { n: 6, s: 'C', name: 'Carbon' }, 
    { n: 7, s: 'N', name: 'Nitrogen' }, { n: 8, s: 'O', name: 'Oxygen' }, { n: 9, s: 'F', name: 'Fluorine' }, 
    { n: 10, s: 'Ne', name: 'Neon' }, { n: 11, s: 'Na', name: 'Sodium' }, { n: 12, s: 'Mg', name: 'Magnesium' }, 
    { n: 13, s: 'Al', name: 'Aluminium' }, { n: 14, s: 'Si', name: 'Silicon' }, { n: 15, s: 'P', name: 'Phosphorus' }, 
    { n: 16, s: 'S', name: 'Sulfur' }, { n: 17, s: 'Cl', name: 'Chlorine' }, { n: 18, s: 'Ar', name: 'Argon' }, 
    { n: 19, s: 'K', name: 'Potassium' }, { n: 20, s: 'Ca', name: 'Calcium' }, { n: 21, s: 'Sc', name: 'Scandium' }, 
    { n: 22, s: 'Ti', name: 'Titanium' }, { n: 23, s: 'V', name: 'Vanadium' }, { n: 24, s: 'Cr', name: 'Chromium' }, 
    { n: 25, s: 'Mn', name: 'Manganese' }, { n: 26, s: 'Fe', name: 'Iron' }, { n: 27, s: 'Co', name: 'Cobalt' }, 
    { n: 28, s: 'Ni', name: 'Nickel' }, { n: 29, s: 'Cu', name: 'Copper' }, { n: 30, s: 'Zn', name: 'Zinc' }, 
    { n: 31, s: 'Ga', name: 'Gallium' }, { n: 32, s: 'Ge', name: 'Germanium' }, { n: 33, s: 'As', name: 'Arsenic' }, 
    { n: 34, s: 'Se', name: 'Selenium' }, { n: 35, s: 'Br', name: 'Bromine' }, { n: 36, s: 'Kr', name: 'Krypton' }, 
    { n: 37, s: 'Rb', name: 'Rubidium' }, { n: 38, s: 'Sr', name: 'Strontium' }, { n: 39, s: 'Y', name: 'Yttrium' }, 
    { n: 40, s: 'Zr', name: 'Zirconium' }, { n: 41, s: 'Nb', name: 'Niobium' }, { n: 42, s: 'Mo', name: 'Molybdenum' }, 
    { n: 43, s: 'Tc', name: 'Technetium' }, { n: 44, s: 'Ru', name: 'Ruthenium' }, { n: 45, s: 'Rh', name: 'Rhodium' }, 
    { n: 46, s: 'Pd', name: 'Palladium' }, { n: 47, s: 'Ag', name: 'Silver' }, { n: 48, s: 'Cd', name: 'Cadmium' }, 
    { n: 49, s: 'In', name: 'Indium' }, { n: 50, s: 'Sn', name: 'Tin' }, { n: 51, s: 'Sb', name: 'Antimony' }, 
    { n: 52, s: 'Te', name: 'Tellurium' }, { n: 53, s: 'I', name: 'Iodine' }, { n: 54, s: 'Xe', name: 'Xenon' }, 
    { n: 55, s: 'Cs', name: 'Caesium' }, { n: 56, s: 'Ba', name: 'Barium' }, { n: 57, s: 'La', name: 'Lanthanum' }, 
    { n: 58, s: 'Ce', name: 'Cerium' }, { n: 59, s: 'Pr', name: 'Praseodymium' }, { n: 60, s: 'Nd', name: 'Neodymium' }, 
    { n: 61, s: 'Pm', name: 'Promethium' }, { n: 62, s: 'Sm', name: 'Samarium' }, { n: 63, s: 'Eu', name: 'Europium' }, 
    { n: 64, s: 'Gd', name: 'Gadolinium' }, { n: 65, s: 'Tb', name: 'Terbium' }, { n: 66, s: 'Dy', name: 'Dysprosium' }, 
    { n: 67, s: 'Ho', name: 'Holmium' }, { n: 68, s: 'Er', name: 'Erbium' }, { n: 69, s: 'Tm', name: 'Thulium' }, 
    { n: 70, s: 'Yb', name: 'Ytterbium' }, { n: 71, s: 'Lu', name: 'Lutetium' }, { n: 72, s: 'Hf', name: 'Hafnium' }, 
    { n: 73, s: 'Ta', name: 'Tantalum' }, { n: 74, s: 'W', name: 'Tungsten' }, { n: 75, s: 'Re', name: 'Rhenium' }, 
    { n: 76, s: 'Os', name: 'Osmium' }, { n: 77, s: 'Ir', name: 'Iridium' }, { n: 78, s: 'Pt', name: 'Platinum' }, 
    { n: 79, s: 'Au', name: 'Gold' }, { n: 80, s: 'Hg', name: 'Mercury' }, { n: 81, s: 'Tl', name: 'Thallium' }, 
    { n: 82, s: 'Pb', name: 'Lead' }, { n: 83, s: 'Bi', name: 'Bismuth' }, { n: 84, s: 'Po', name: 'Polonium' }, 
    { n: 85, s: 'At', name: 'Astatine' }, { n: 86, s: 'Rn', name: 'Radon' }, { n: 87, s: 'Fr', name: 'Francium' }, 
    { n: 88, s: 'Ra', name: 'Radium' }, { n: 89, s: 'Ac', name: 'Actinium' }, { n: 90, s: 'Th', name: 'Thorium' }, 
    { n: 91, s: 'Pa', name: 'Protactinium' }, { n: 92, s: 'U', name: 'Uranium' }, { n: 93, s: 'Np', name: 'Neptunium' }, 
    { n: 94, s: 'Pu', name: 'Plutonium' }, { n: 95, s: 'Am', name: 'Americium' }, { n: 96, s: 'Cm', name: 'Curium' }, 
    { n: 97, s: 'Bk', name: 'Berkelium' }, { n: 98, s: 'Cf', name: 'Californium' }, { n: 99, s: 'Es', name: 'Einsteinium' }, 
    { n: 100, s: 'Fm', name: 'Fermium' }, { n: 101, s: 'Md', name: 'Mendelevium' }, { n: 102, s: 'No', name: 'Nobelium' }, 
    { n: 103, s: 'Lr', name: 'Lawrencium' }, { n: 104, s: 'Rf', name: 'Rutherfordium' }, { n: 105, s: 'Db', name: 'Dubnium' }, 
    { n: 106, s: 'Sg', name: 'Seaborgium' }, { n: 107, s: 'Bh', name: 'Bohrium' }, { n: 108, s: 'Hs', name: 'Hassium' }, 
    { n: 109, s: 'Mt', name: 'Meitnerium' }, { n: 110, s: 'Ds', name: 'Darmstadtium' }, { n: 111, s: 'Rg', name: 'Roentgenium' }, 
    { n: 112, s: 'Cn', name: 'Copernicium' }, { n: 113, s: 'Nh', name: 'Nihonium' }, { n: 114, s: 'Fl', name: 'Flerovium' }, 
    { n: 115, s: 'Mc', name: 'Moscovium' }, { n: 116, s: 'Lv', name: 'Livermorium' }, { n: 117, s: 'Ts', name: 'Tennessine' }, 
    { n: 118, s: 'Og', name: 'Oganesson' }
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

const ASCII_DATA = [
    { char: "A", bin: "01000001" }, { char: "B", bin: "01000010" }, { char: "C", bin: "01000011" },
    { char: "D", bin: "01000100" }, { char: "E", bin: "01000101" }, { char: "F", bin: "01000110" },
    { char: "G", bin: "01000111" }, { char: "H", bin: "01001000" }, { char: "I", bin: "01001001" },
    { char: "J", bin: "01001010" }, { char: "K", bin: "01001011" }, { char: "L", bin: "01001100" },
    { char: "M", bin: "01001101" }, { char: "N", bin: "01001110" }, { char: "O", bin: "01001111" },
    { char: "P", bin: "01010000" }, { char: "Q", bin: "01010001" }, { char: "R", bin: "01010010" },
    { char: "S", bin: "01010011" }, { char: "T", bin: "01010100" }, { char: "U", bin: "01010101" },
    { char: "V", bin: "01010110" }, { char: "W", bin: "01010111" }, { char: "X", bin: "01011000" },
    { char: "Y", bin: "01011001" }, { char: "Z", bin: "01011010" },
    { char: "a", bin: "01100001" }, { char: "b", bin: "01100010" }, { char: "c", bin: "01100011" }
];

const TLD_DATA = [
    { tld: ".tv", country: "Tuvalu", fact: "Tuvalu earns about 10% of its GDP from licensing this domain." },
    { tld: ".io", country: "British Indian Ocean Territory", fact: "Popular with tech startups for Input/Output." },
    { tld: ".ai", country: "Anguilla", fact: "A favorite for Artificial Intelligence companies." },
    { tld: ".me", country: "Montenegro", fact: "Widely used for personal blogs and portfolios." },
    { tld: ".co", country: "Colombia", fact: "Often used as an alternative to .com." },
    { tld: ".ly", country: "Libya", fact: "Used for creative domain hacks like bit.ly." },
    { tld: ".fm", country: "Federated States of Micronesia", fact: "Popular for radio stations and podcasts." },
    { tld: ".gg", country: "Guernsey", fact: "Adopted by the gaming community for 'Good Game'." },
    { tld: ".dj", country: "Djibouti", fact: "Used by disc jockeys and music sites." },
    { tld: ".to", country: "Tonga", fact: "Used for 'to' domain hacks like go.to." },
    { tld: ".ws", country: "Samoa", fact: "Marketed as 'Web Site'." },
    { tld: ".nu", country: "Niue", fact: "Means 'now' in Swedish, Danish, and Dutch." },
    { tld: ".tk", country: "Tokelau", fact: "Known for being free to register for years." },
    { tld: ".vc", country: "Saint Vincent and the Grenadines", fact: "Used by Venture Capital firms." },
    { tld: ".ag", country: "Antigua and Barbuda", fact: "Used for agricultural businesses." },
    { tld: ".sc", country: "Seychelles", fact: "Sometimes used for source code related sites." },
    { tld: ".ac", country: "Ascension Island", fact: "Popular for academic institutions." },
    { tld: ".ch", country: "Switzerland", fact: "From the Latin 'Confoederatio Helvetica'." },
    { tld: ".de", country: "Germany", fact: "Deutschland. One of the most popular ccTLDs." },
    { tld: ".uk", country: "United Kingdom", fact: "Shorter alternative to .co.uk." }
];

const CALLING_CODES = [
    { code: "+1", country: "United States / Canada" }, { code: "+44", country: "United Kingdom" },
    { code: "+33", country: "France" }, { code: "+49", country: "Germany" }, { code: "+81", country: "Japan" },
    { code: "+86", country: "China" }, { code: "+91", country: "India" }, { code: "+7", country: "Russia" },
    { code: "+61", country: "Australia" }, { code: "+55", country: "Brazil" }, { code: "+39", country: "Italy" },
    { code: "+34", country: "Spain" }, { code: "+52", country: "Mexico" }, { code: "+27", country: "South Africa" },
    { code: "+20", country: "Egypt" }, { code: "+31", country: "Netherlands" }, { code: "+32", country: "Belgium" },
    { code: "+41", country: "Switzerland" }, { code: "+46", country: "Sweden" }, { code: "+47", country: "Norway" },
    { code: "+48", country: "Poland" }, { code: "+64", country: "New Zealand" }, { code: "+353", country: "Ireland" },
    { code: "+90", country: "Turkey" }, { code: "+82", country: "South Korea" }, { code: "+65", country: "Singapore" },
    { code: "+351", country: "Portugal" }, { code: "+30", country: "Greece" }, { code: "+54", country: "Argentina" },
    { code: "+57", country: "Colombia" }, { code: "+62", country: "Indonesia" }, { code: "+63", country: "Philippines" },
    { code: "+66", country: "Thailand" }, { code: "+84", country: "Vietnam" }, { code: "+92", country: "Pakistan" },
    { code: "+98", country: "Iran" }, { code: "+234", country: "Nigeria" }, { code: "+254", country: "Kenya" }
];

const CURRENCIES = [
    {
        id: "EUR",
        name: "Euro (€)",
        members: ["Austria", "Belgium", "Croatia", "Cyprus", "Estonia", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Portugal", "Slovakia", "Slovenia", "Spain"]
    },
    {
        id: "USD",
        name: "US Dollar ($)",
        members: ["United States", "Ecuador", "El Salvador", "Panama", "Timor-Leste", "Zimbabwe", "Puerto Rico"]
    },
    {
        id: "GBP",
        name: "Pound Sterling (£)",
        members: ["United Kingdom", "Jersey", "Guernsey", "Isle of Man", "Gibraltar", "Falkland Islands"]
    },
    {
        id: "CHF",
        name: "Swiss Franc (Fr)",
        members: ["Switzerland", "Liechtenstein"]
    },
    {
        id: "XOF",
        name: "West African CFA Franc",
        members: ["Benin", "Burkina Faso", "Ivory Coast", "Guinea-Bissau", "Mali", "Niger", "Senegal", "Togo"]
    },
    {
        id: "XAF",
        name: "Central African CFA Franc",
        members: ["Cameroon", "Central African Republic", "Chad", "Republic of the Congo", "Equatorial Guinea", "Gabon"]
    },
    {
        id: "AUD",
        name: "Australian Dollar ($)",
        members: ["Australia", "Kiribati", "Nauru", "Tuvalu"]
    },
    {
        id: "NZD",
        name: "New Zealand Dollar ($)",
        members: ["New Zealand", "Cook Islands", "Niue", "Tokelau", "Pitcairn Islands"]
    },
    {
        id: "XCD",
        name: "East Caribbean Dollar ($)",
        members: ["Antigua and Barbuda", "Dominica", "Grenada", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent", "Anguilla", "Montserrat"]
    }
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
    { c: "Europe", n: "Ireland", code: "ie", capital: "Dublin" }, { c: "Europe", n: "Norway", code: "no", capital: "Oslo" },
    { c: "Europe", n: "Finland", code: "fi", capital: "Helsinki" }, { c: "Europe", n: "Denmark", code: "dk", capital: "Copenhagen" },
    { c: "Europe", n: "Hungary", code: "hu", capital: "Budapest" }, { c: "Europe", n: "Czech Republic", code: "cz", capital: "Prague" },
    { c: "Asia", n: "Philippines", code: "ph", capital: "Manila" }, { c: "Asia", n: "Pakistan", code: "pk", capital: "Islamabad" },
    { c: "South America", n: "Colombia", code: "co", capital: "Bogotá" }, { c: "South America", n: "Peru", code: "pe", capital: "Lima" },
    { c: "Africa", n: "Morocco", code: "ma", capital: "Rabat" }, { c: "Africa", n: "Ethiopia", code: "et", capital: "Addis Ababa" },
    { c: "Europe", n: "Greece", code: "gr", capital: "Athens" }, { c: "Europe", n: "Romania", code: "ro", capital: "Bucharest" },
    { c: "Europe", n: "Iceland", code: "is", capital: "Reykjavik" }, { c: "Europe", n: "Croatia", code: "hr", capital: "Zagreb" },
    { c: "Asia", n: "Turkey", code: "tr", capital: "Ankara" }, { c: "Asia", n: "Saudi Arabia", code: "sa", capital: "Riyadh" },
    { c: "Asia", n: "United Arab Emirates", code: "ae", capital: "Abu Dhabi" }, { c: "Asia", n: "Israel", code: "il", capital: "Jerusalem" },
    { c: "Asia", n: "Iran", code: "ir", capital: "Tehran" }, { c: "Asia", n: "Iraq", code: "iq", capital: "Baghdad" },
    { c: "Asia", n: "Afghanistan", code: "af", capital: "Kabul" }, { c: "Asia", n: "Bangladesh", code: "bd", capital: "Dhaka" },
    { c: "Asia", n: "Sri Lanka", code: "lk", capital: "Colombo" }, { c: "Asia", n: "Nepal", code: "np", capital: "Kathmandu" },
    { c: "Asia", n: "Mongolia", code: "mn", capital: "Ulaanbaatar" }, { c: "Asia", n: "Kazakhstan", code: "kz", capital: "Astana" },
    { c: "Africa", n: "Algeria", code: "dz", capital: "Algiers" }, { c: "Africa", n: "Sudan", code: "sd", capital: "Khartoum" },
    { c: "Africa", n: "Ghana", code: "gh", capital: "Accra" }, { c: "Africa", n: "DR Congo", code: "cd", capital: "Kinshasa" },
    { c: "Africa", n: "Tanzania", code: "tz", capital: "Dodoma" }, { c: "Africa", n: "Angola", code: "ao", capital: "Luanda" },
    { c: "North America", n: "Cuba", code: "cu", capital: "Havana" }, { c: "North America", n: "Jamaica", code: "jm", capital: "Kingston" },
    { c: "North America", n: "Costa Rica", code: "cr", capital: "San José" }, { c: "North America", n: "Panama", code: "pa", capital: "Panama City" },
    { c: "South America", n: "Venezuela", code: "ve", capital: "Caracas" }, { c: "South America", n: "Ecuador", code: "ec", capital: "Quito" },
    { c: "South America", n: "Bolivia", code: "bo", capital: "Sucre" }, { c: "South America", n: "Paraguay", code: "py", capital: "Asunción" },
    { c: "South America", n: "Uruguay", code: "uy", capital: "Montevideo" }
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
    { c: "Jamaica", d: "Ackee and Saltfish", code: "jm" }, { c: "Ireland", d: "Irish Stew", code: "ie" },
    { c: "Scotland", d: "Haggis", code: "gb-sct" }, { c: "Wales", d: "Cawl", code: "gb-wls" },
    { c: "Ethiopia", d: "Injera", code: "et" }, { c: "Nigeria", d: "Jollof Rice", code: "ng" }
];

const POPULATION = [
    { c: "China", p: 1412 }, { c: "India", p: 1428 }, { c: "USA", p: 335 }, { c: "Indonesia", p: 277 },
    { c: "Pakistan", p: 240 }, { c: "Nigeria", p: 224 }, { c: "Brazil", p: 216 }, { c: "Bangladesh", p: 173 },
    { c: "Russia", p: 144 }, { c: "Mexico", p: 128 }, { c: "Japan", p: 123 }, { c: "Ethiopia", p: 126 },
    { c: "Philippines", p: 117 }, { c: "Egypt", p: 113 }, { c: "Vietnam", p: 99 }, { c: "DR Congo", p: 102 },
    { c: "Turkey", p: 86 }, { c: "Iran", p: 89 }, { c: "Germany", p: 84 }, { c: "Thailand", p: 72 },
    { c: "United Kingdom", p: 68 }, { c: "France", p: 68 }, { c: "Italy", p: 59 }, { c: "South Africa", p: 60 },
    { c: "Tanzania", p: 67 }, { c: "Myanmar", p: 54 }, { c: "South Korea", p: 52 }, { c: "Colombia", p: 52 },
    { c: "Kenya", p: 55 }, { c: "Spain", p: 48 }, { c: "Argentina", p: 46 }, { c: "Ukraine", p: 37 },
    { c: "Canada", p: 39 }, { c: "Poland", p: 38 }, { c: "Morocco", p: 38 }, { c: "Saudi Arabia", p: 37 },
    { c: "Uzbekistan", p: 36 }, { c: "Peru", p: 34 }, { c: "Malaysia", p: 34 }, { c: "Angola", p: 37 }, { c: "Ghana", p: 34 }, 
    { c: "Yemen", p: 34 }, { c: "Nepal", p: 31 }, { c: "Venezuela", p: 29 }, { c: "Australia", p: 26 }, { c: "North Korea", p: 26 }, 
    { c: "Taiwan", p: 24 }, { c: "Netherlands", p: 18 }, { c: "Chile", p: 20 }, { c: "Ecuador", p: 18 }, { c: "Guatemala", p: 18 }, 
    { c: "Senegal", p: 18 }, { c: "Cambodia", p: 17 }, { c: "Zimbabwe", p: 16 }, { c: "Chad", p: 18 }, { c: "Somalia", p: 18 }, 
    { c: "Guinea", p: 14 }, { c: "Rwanda", p: 14 }, { c: "Benin", p: 13 }, { c: "Burundi", p: 13 }, { c: "Tunisia", p: 12 }, 
    { c: "Bolivia", p: 12 }, { c: "Haiti", p: 12 }, { c: "Belgium", p: 12 }, { c: "Jordan", p: 11 }, { c: "Dominican Republic", p: 11 }, 
    { c: "Cuba", p: 11 }, { c: "Sweden", p: 11 }, { c: "Portugal", p: 10 }, { c: "Greece", p: 10 }, { c: "Czech Republic", p: 10 }, 
    { c: "Hungary", p: 10 }, { c: "United Arab Emirates", p: 10 }, { c: "Belarus", p: 9 }, { c: "Israel", p: 9 }, { c: "Austria", p: 9 }, 
    { c: "Switzerland", p: 9 }, { c: "Honduras", p: 10 }, { c: "Papua New Guinea", p: 10 }, { c: "Togo", p: 9 }, { c: "Sierra Leone", p: 9 }, 
    { c: "Laos", p: 8 }, { c: "Paraguay", p: 7 }, { c: "Libya", p: 7 }, { c: "El Salvador", p: 6 }, { c: "Nicaragua", p: 7 }, 
    { c: "Kyrgyzstan", p: 7 }, { c: "Turkmenistan", p: 6 }, { c: "Singapore", p: 6 }, { c: "Denmark", p: 6 }, { c: "Finland", p: 6 }, 
    { c: "Slovakia", p: 5 }, { c: "Norway", p: 5 }, { c: "Ireland", p: 5 }
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
    },
    {
        id: "AU",
        name: "African Union",
        full: "A continental union of 55 member states in Africa.",
        members: [
            "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros", "Congo", "Djibouti", "DR Congo", "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe"
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

// Refined Constellation Coordinates (0-100 Grid)
const CONSTELLATIONS = [
    {
        name: "Ursa Major (Big Dipper)",
        desc: "The Great Bear. The Big Dipper is an asterism within it.",
        stars: [
            [10, 30],  // Alkaid (Handle Tip)
            [25, 35],  // Mizar (Handle)
            [35, 38],  // Alioth (Handle/Bowl join area)
            [50, 40],  // Megrez (Bowl Top-Left)
            [50, 60],  // Phecda (Bowl Bottom-Left)
            [75, 60],  // Merak (Bowl Bottom-Right)
            [75, 40]   // Dubhe (Bowl Top-Right)
        ],
        lines: [[0, 1], [1, 2], [2, 3], [3, 6], [6, 5], [5, 4], [4, 3]] 
    },
    {
        name: "Orion",
        desc: "The Hunter. Recognizable by the belt of three stars.",
        stars: [
            [35, 20], // Betelgeuse
            [65, 25], // Bellatrix
            [45, 52], // Alnitak
            [50, 50], // Alnilam
            [55, 48], // Mintaka
            [35, 85], // Saiph
            [70, 80], // Rigel
            [50, 10]  // Meissa
        ],
        lines: [[0, 2], [1, 4], [2, 3], [3, 4], [2, 5], [4, 6], [5, 6], [0, 7], [1, 7]]
    },
    {
        name: "Cassiopeia",
        desc: "The Queen. The distinctive 'W' or 'M' shape.",
        stars: [
            [15, 30], // Epsilon
            [35, 60], // Delta
            [50, 40], // Gamma
            [65, 70], // Alpha
            [85, 50]  // Beta
        ],
        lines: [[0, 1], [1, 2], [2, 3], [3, 4]]
    },
    {
        name: "Leo",
        desc: "The Lion. Featuring the 'Sickle' shape.",
        stars: [
            [60, 70], // Regulus
            [55, 55], // Eta
            [60, 40], // Gamma
            [70, 30], // Zeta
            [75, 35], // Mu
            [75, 45], // Epsilon
            [20, 50], // Denebola
            [35, 60]  // Zosma
        ],
        lines: [[0, 1], [1, 2], [2, 3], [3, 4], [2, 5], [5, 4], [2, 7], [7, 6], [0, 7]]
    },
    {
        name: "Gemini",
        desc: "The Twins, Castor and Pollux.",
        stars: [
            [30, 15], // Castor (Head 1)
            [65, 20], // Pollux (Head 2)
            [30, 45], // Mebsuta (Waist 1)
            [65, 50], // Wasat (Waist 2)
            [25, 80], // Tejat (Foot 1)
            [70, 80], // Alhena (Foot 2)
            [30, 25], // Shoulder 1
            [65, 28]  // Shoulder 2
        ],
        lines: [[0, 2], [2, 4], [1, 3], [3, 5], [2, 3]] // Two parallel lines roughly
    },
    {
        name: "Scorpius",
        desc: "The Scorpion. A winding 'J' shape.",
        stars: [
            [85, 20], // Graffias
            [80, 25], // Dschubba
            [70, 35], // Antares
            [60, 55], // Tau Sco
            [50, 75], // Epsilon Sco
            [40, 85], // Mu Sco
            [30, 90], // Zeta Sco
            [20, 80], // Sargas
            [15, 65], // Shaula
            [10, 60]  // Lesath
        ],
        lines: [[0, 1], [1, 2], [2, 3], [3, 4], [4, 5], [5, 6], [6, 7], [7, 8], [8, 9]]
    }
];

const KEYBOARD_REGIONS = [
    { name: "QWERTY", rows: ["Q W E R T Y U I O P", "A S D F G H J K L", "Z X C V B N M"], region: "United States / English", desc: "The standard layout for English-speaking regions." },
    { name: "AZERTY", rows: ["A Z E R T Y U I O P", "Q S D F G H J K L M", "W X C V B N"], region: "France / French", desc: "Standard in France and Belgium. Note the A/Q and Z/W swaps." },
    { name: "QWERTZ", rows: ["Q W E R T Z U I O P Ü", "A S D F G H J K L Ö Ä", "Y X C V B N M"], region: "Germany / German", desc: "Used in Germany and much of Central Europe. Note the Y/Z swap." },
    { name: "Russian", rows: ["Й Ц У К Е Н Г Ш Щ З Х Ъ", "Ф Ы В А П Р О Л Д Ж Э", "Я Ч С М И Т Ь Б Ю"], region: "Russia / Russian", desc: "The standard Cyrillic layout in Russia." },
    { name: "Greek", rows: ["; Ε Ρ Τ Υ Θ Ι Ο Π", "Α Σ Δ Φ Γ Η Ξ Κ Λ", "Ζ Χ Ψ Ω Β Ν Μ"], region: "Greece / Greek", desc: "The standard layout for the Greek language." },
    { name: "Turkish Q", rows: ["Q W E R T Y U I O P Ğ Ü", "A S D F G H J K L Ş İ", "Z X C V B N M Ö Ç"], region: "Turkey / Turkish", desc: "A QWERTY variant with letters for the Turkish alphabet." },
    { name: "Ukrainian", rows: ["Й Ц У К Е Н Г Ш Щ З Х Ї", "Ф І В А П Р О Л Д Ж Є", "Я Ч С М И Т Ь Б Ю"], region: "Ukraine / Ukrainian", desc: "Cyrillic layout for Ukrainian, similar to Russian but with unique letters like Ї, І, Є." },
    { name: "Swiss", rows: ["Q W E R T Z U I O P Ü", "A S D F G H J K L Ö Ä", "Y X C V B N M"], region: "Switzerland", desc: "A QWERTZ layout for Swiss languages." },
    { name: "Hebrew", rows: ["/ ' ק ר א ט ו ן ם פ", "ש ד ג כ ע י ח ל ך ף", "ז ס ב ה נ מ צ ת ץ"], region: "Israel / Hebrew", desc: "Standard Hebrew layout." },
    { name: "Arabic", rows: ["ض ص ث ق ف غ ع ه خ ح ج د", "ش س ي ب ل ا ت ن م ك ط", "ئ ء ؤ ر ى ة و ز ظ"], region: "Arabic-speaking countries", desc: "A common Arabic layout." }
];

const MUSIC_NOTES_DATA = [
    // Treble Clef
    { clef: 'treble', name: 'E4', y: 80, label: 'E' }, { clef: 'treble', name: 'F4', y: 75, label: 'F' },
    { clef: 'treble', name: 'G4', y: 70, label: 'G' }, { clef: 'treble', name: 'A4', y: 65, label: 'A' },
    { clef: 'treble', name: 'B4', y: 60, label: 'B' }, { clef: 'treble', name: 'C5', y: 55, label: 'C' },
    { clef: 'treble', name: 'D5', y: 50, label: 'D' }, { clef: 'treble', name: 'E5', y: 45, label: 'E' },
    { clef: 'treble', name: 'F5', y: 40, label: 'F' },
    { clef: 'treble', name: 'C4', y: 90, ledger: 'down', label: 'C' }, // Middle C
    { clef: 'treble', name: 'G5', y: 35, label: 'G' },
    { clef: 'treble', name: 'A5', y: 30, ledger: 'up', label: 'A' },

    // Bass Clef
    { clef: 'bass', name: 'G2', y: 80, label: 'G' }, { clef: 'bass', name: 'A2', y: 75, label: 'A' },
    { clef: 'bass', name: 'B2', y: 70, label: 'B' }, { clef: 'bass', name: 'C3', y: 65, label: 'C' },
    { clef: 'bass', name: 'D3', y: 60, label: 'D' }, { clef: 'bass', name: 'E3', y: 55, label: 'E' },
    { clef: 'bass', name: 'F3', y: 50, label: 'F' }, { clef: 'bass', name: 'G3', y: 45, label: 'G' },
    { clef: 'bass', name: 'A3', y: 40, label: 'A' },
    { clef: 'bass', name: 'C4', y: 30, ledger: 'up', label: 'C' }, // Middle C
    { clef: 'bass', name: 'F2', y: 85, label: 'F' },
    { clef: 'bass', name: 'B1', y: 95, ledger: 'down', label: 'B' },
];

const HISTORICAL_EVENTS_BY_DATE = {
    // January
    '1-1': { year: 1863, event: "Emancipation Proclamation", desc: "President Abraham Lincoln issued the Emancipation Proclamation, declaring slaves in Confederate states to be free." },
    '1-2': { year: 1974, event: "55 mph Speed Limit", desc: "U.S. President Richard Nixon signs the National Maximum Speed Law, setting a maximum speed limit of 55 mph to conserve gasoline during an OPEC embargo." },
    '1-3': { year: 1959, event: "Alaska Statehood", desc: "Alaska is admitted as the 49th U.S. state." },
    '1-4': { year: 1847, event: "Colt Sells First Revolvers", desc: "Samuel Colt sells his first revolver pistols to the United States government." },
    '1-5': { year: 1933, event: "Golden Gate Bridge Construction", desc: "Construction begins on the Golden Gate Bridge in San Francisco Bay." },
    '1-6': { year: 1912, event: "New Mexico Statehood", desc: "New Mexico is admitted as the 47th U.S. state." },
    '1-7': { year: 1927, event: "First Transatlantic Phone Call", desc: "The first transatlantic commercial telephone call is made from New York City to London." },
    '1-8': { year: 1815, event: "Battle of New Orleans", desc: "U.S. forces led by Andrew Jackson defeat the British in the final major battle of the War of 1812." },
    '1-9': { year: 2007, event: "iPhone Unveiled", desc: "Apple CEO Steve Jobs unveils the first iPhone at the Macworld convention." },
    '1-10': { year: 1920, event: "League of Nations Founded", desc: "The League of Nations holds its first meeting, formally ending World War I with the ratification of the Treaty of Versailles." },
    '1-11': { year: 1964, event: "Surgeon General's Report on Smoking", desc: "U.S. Surgeon General Luther Terry releases a landmark report linking smoking to lung cancer." },
    '1-12': { year: 1998, event: "Human Cloning Banned in Europe", desc: "19 European nations sign a protocol to the Convention on Human Rights and Biomedicine, prohibiting human cloning." },
    '1-13': { year: 1898, event: "J'Accuse...!", desc: "Émile Zola's open letter 'J'Accuse...!' is published, exposing the Dreyfus affair in France." },
    '1-14': { year: 1952, event: "The 'Today' Show Premieres", desc: "NBC's 'Today' show, the first of its kind, makes its debut on American television." },
    '1-15': { year: 2001, event: "Wikipedia Launched", desc: "The free, open-content online encyclopedia Wikipedia is launched by Jimmy Wales and Larry Sanger." },
    '1-16': { year: 1920, event: "Prohibition Begins", desc: "The 18th Amendment to the U.S. Constitution goes into effect, beginning the era of Prohibition." },
    '1-17': { year: 1773, event: "Crossing the Antarctic Circle", desc: "Captain James Cook and his crew become the first Europeans to cross the Antarctic Circle." },
    '1-18': { year: 1778, event: "Discovery of Hawaii", desc: "Captain James Cook becomes the first European to discover the Hawaiian Islands, which he names the 'Sandwich Islands'." },
    '1-19': { year: 1966, event: "Indira Gandhi Elected PM", desc: "Indira Gandhi is elected as Prime Minister of India, becoming the country's first female leader." },
    '1-20': { year: 1961, event: "JFK's Inauguration", desc: "John F. Kennedy is inaugurated as the 35th U.S. President, famously saying, 'Ask not what your country can do for you...'" },
    '1-21': { year: 1793, event: "Execution of Louis XVI", desc: "King Louis XVI of France is executed by guillotine in Paris during the French Revolution." },
    '1-22': { year: 1973, event: "Roe v. Wade Decision", desc: "The U.S. Supreme Court delivers its landmark Roe v. Wade decision, legalizing abortion nationwide." },
    '1-23': { year: 1964, event: "24th Amendment Ratified", desc: "The 24th Amendment to the U.S. Constitution, prohibiting poll taxes in federal elections, is ratified." },
    '1-24': { year: 1848, event: "California Gold Rush Begins", desc: "James W. Marshall discovers gold at Sutter's Mill, sparking the California Gold Rush." },
    '1-25': { year: 1924, event: "First Winter Olympics", desc: "The first Winter Olympic Games open in Chamonix, France." },
    '1-26': { year: 1788, event: "Australia Day", desc: "The British First Fleet, led by Arthur Phillip, lands at Sydney Cove, establishing the first permanent European settlement in Australia." },
    '1-27': { year: 1945, event: "Auschwitz Liberated", desc: "The Soviet Red Army liberates the Auschwitz-Birkenau concentration camps in Poland." },
    '1-28': { year: 1986, event: "Challenger Disaster", desc: "The Space Shuttle Challenger breaks apart 73 seconds after liftoff, killing all seven astronauts." },
    '1-29': { year: 1886, event: "First Gasoline-Powered Car", desc: "Karl Benz patents the Benz Patent-Motorwagen, the first successful gasoline-driven automobile." },
    '1-30': { year: 1948, event: "Assassination of Mahatma Gandhi", desc: "Indian independence leader Mahatma Gandhi is assassinated in New Delhi." },
    '1-31': { year: 1958, event: "First U.S. Satellite", desc: "The United States launches its first satellite, Explorer 1, into orbit." },
    // February
    '2-1': { year: 2003, event: "Space Shuttle Columbia Disaster", desc: "Space Shuttle Columbia disintegrates upon re-entry, killing all seven astronauts." },
    '2-2': { year: 1887, event: "First Groundhog Day", desc: "The first official Groundhog Day is celebrated in Punxsutawney, Pennsylvania." },
    '2-3': { year: 1959, event: "The Day the Music Died", desc: "Musicians Buddy Holly, Ritchie Valens, and The Big Bopper die in a plane crash in Iowa." },
    '2-4': { year: 2004, event: "Facebook Launched", desc: "Mark Zuckerberg launches 'Thefacebook', a social networking site for Harvard students, which would later become Facebook." },
    '2-5': { year: 1999, event: "U.S. Senate Acquits Bill Clinton", desc: "The U.S. Senate acquits President Bill Clinton on charges of perjury and obstruction of justice in his impeachment trial." },
    '2-6': { year: 1952, event: "Queen Elizabeth II Ascends", desc: "Princess Elizabeth becomes Queen Elizabeth II of the United Kingdom upon the death of her father, King George VI." },
    '2-7': { year: 1964, event: "The Beatles Arrive in America", desc: "The Beatles land at JFK Airport in New York, beginning the 'British Invasion' of American music." },
    '2-8': { year: 1910, event: "Boy Scouts of America Founded", desc: "The Boy Scouts of America is founded by William D. Boyce." },
    '2-9': { year: 1964, event: "The Beatles on Ed Sullivan", desc: "The Beatles make their first live American television appearance on The Ed Sullivan Show to a record-breaking audience." },
    '2-10': { year: 1996, event: "Deep Blue vs. Kasparov", desc: "IBM's chess computer Deep Blue defeats world champion Garry Kasparov for the first time in a single game." },
    '2-11': { year: 1990, event: "Nelson Mandela Released", desc: "Anti-apartheid leader Nelson Mandela is released from prison in South Africa after 27 years." },
    '2-12': { year: 1909, event: "NAACP Founded", desc: "The National Association for the Advancement of Colored People (NAACP) is founded." },
    '2-13': { year: 1945, event: "Dresden Bombing", desc: "Allied forces begin the controversial bombing of Dresden, Germany, during World War II." },
    '2-14': { year: 2005, event: "YouTube Launched", desc: "The video-sharing website YouTube is founded by three former PayPal employees." },
    '2-15': { year: 1965, event: "Canada's Maple Leaf Flag Debuts", desc: "Canada adopts the iconic red and white Maple Leaf flag." },
    '2-16': { year: 1959, event: "Fidel Castro Sworn In", desc: "Fidel Castro is sworn in as Prime Minister of Cuba after overthrowing Fulgencio Batista." },
    '2-17': { year: 1923, event: "Tutankhamun's Tomb Unsealed", desc: "Archaeologist Howard Carter unseals the burial chamber of Pharaoh Tutankhamun." },
    '2-18': { year: 1930, event: "Pluto Discovered", desc: "Clyde Tombaugh discovers the dwarf planet Pluto at the Lowell Observatory in Arizona." },
    '2-19': { year: 1942, event: "Japanese-American Internment", desc: "U.S. President Franklin D. Roosevelt signs Executive Order 9066, authorizing the internment of Japanese Americans." },
    '2-20': { year: 1962, event: "John Glenn Orbits Earth", desc: "John Glenn becomes the first American to orbit the Earth aboard the Friendship 7 spacecraft." },
    '2-21': { year: 1848, event: "The Communist Manifesto Published", desc: "Karl Marx and Friedrich Engels publish 'The Communist Manifesto' in London." },
    '2-22': { year: 1980, event: "Miracle on Ice", desc: "The U.S. Olympic hockey team defeats the heavily favored Soviet team in a major upset at the Winter Olympics." },
    '2-23': { year: 1945, event: "Flag Raising on Iwo Jima", desc: "U.S. Marines raise the American flag on Mount Suribachi during the Battle of Iwo Jima, an image immortalized by Joe Rosenthal." },
    '2-24': { year: 1821, event: "Mexican Independence", desc: "Mexico declares its independence from Spain with the Plan of Iguala." },
    '2-25': { year: 1986, event: "People Power Revolution", desc: "Philippine President Ferdinand Marcos is overthrown by the nonviolent People Power Revolution." },
    '2-26': { year: 1993, event: "World Trade Center Bombing", desc: "A truck bomb detonates in the parking garage of the World Trade Center in New York City." },
    '2-27': { year: 1933, event: "Reichstag Fire", desc: "The German parliament building (Reichstag) is set on fire, an event used by the Nazis to consolidate power." },
    '2-28': { year: 1991, event: "First Gulf War Ends", desc: "President George H. W. Bush declares a ceasefire, ending the Persian Gulf War." },
    '2-29': { year: '45 BC', event: "First Leap Day", desc: "The first leap day was introduced as part of Julius Caesar's calendar reform." },
    // March
    '3-1': { year: 1961, event: "Peace Corps Established", desc: "U.S. President John F. Kennedy issues an executive order establishing the Peace Corps." },
    '3-2': { year: 1931, event: "'The Star-Spangled Banner' Adopted", desc: "The U.S. Congress adopts 'The Star-Spangled Banner' as the national anthem." },
    '3-3': { year: 1923, event: "TIME Magazine First Published", desc: "The first issue of TIME magazine is published." },
    '3-4': { year: 1789, event: "First U.S. Congress Meets", desc: "The first U.S. Congress meets in New York City, putting the Constitution into effect." },
    '3-5': { year: 1770, event: "Boston Massacre", desc: "British soldiers kill five colonists in Boston, an event that escalates tensions leading to the American Revolution." },
    '3-6': { year: 1836, event: "The Alamo Falls", desc: "The Battle of the Alamo ends after a 13-day siege, with Mexican forces defeating the Texan defenders." },
    '3-7': { year: 1965, event: "Bloody Sunday in Selma", desc: "Civil rights marchers are brutally attacked by state troopers on the Edmund Pettus Bridge in Selma, Alabama." },
    '3-8': { year: 1917, event: "February Revolution Begins", desc: "The February Revolution begins in Russia with protests in Petrograd (St. Petersburg), leading to the abdication of Tsar Nicholas II." },
    '3-9': { year: 1959, event: "Barbie Doll Debuts", desc: "The Barbie doll is unveiled at the American International Toy Fair in New York City." },
    '3-10': { year: 1876, event: "First Telephone Call", desc: "Alexander Graham Bell makes the first successful telephone call, saying 'Mr. Watson, come here, I want to see you.'" },
    '3-11': { year: 2020, event: "COVID-19 Declared a Pandemic", desc: "The World Health Organization declares the COVID-19 outbreak a global pandemic." },
    '3-12': { year: 1989, event: "World Wide Web Proposed", desc: "Tim Berners-Lee submits a proposal for an information management system, which would become the World Wide Web." },
    '3-13': { year: 1781, event: "Uranus Discovered", desc: "Astronomer William Herschel discovers the planet Uranus." },
    '3-14': { year: 1879, event: "Albert Einstein Born", desc: "Albert Einstein, the physicist who developed the theory of relativity, is born in Ulm, Germany." },
    '3-15': { year: '44 BC', event: "Assassination of Julius Caesar", desc: "Julius Caesar is assassinated by a group of Roman senators on the Ides of March." },
    '3-16': { year: 1968, event: "My Lai Massacre", desc: "U.S. soldiers kill hundreds of unarmed Vietnamese civilians in the My Lai Massacre during the Vietnam War." },
    '3-17': { year: 461, event: "Death of St. Patrick", desc: "St. Patrick, the patron saint of Ireland, dies, an event now commemorated as St. Patrick's Day." },
    '3-18': { year: 1965, event: "First Spacewalk", desc: "Soviet cosmonaut Alexei Leonov performs the first spacewalk, leaving his Voskhod 2 capsule for 12 minutes." },
    '3-19': { year: 2003, event: "Invasion of Iraq Begins", desc: "The United States and coalition forces begin the invasion of Iraq." },
    '3-20': { year: 1956, event: "Tunisia Gains Independence", desc: "Tunisia gains its independence from France." },
    '3-21': { year: 2006, event: "Twitter Launched", desc: "Jack Dorsey sends the first-ever tweet, 'just setting up my twttr'." },
    '3-22': { year: 1987, event: "First AIDS Drug Approved", desc: "The U.S. FDA approves AZT, the first antiviral drug for the treatment of AIDS." },
    '3-23': { year: 1919, event: "Fascist Party Founded", desc: "Benito Mussolini founds the Fasci di Combattimento, the precursor to the National Fascist Party in Italy." },
    '3-24': { year: 1989, event: "Exxon Valdez Oil Spill", desc: "The oil tanker Exxon Valdez runs aground in Alaska's Prince William Sound, causing a massive oil spill." },
    '3-25': { year: 1911, event: "Triangle Shirtwaist Factory Fire", desc: "A fire at the Triangle Shirtwaist Factory in New York City kills 146 garment workers, leading to improved factory safety standards." },
    '3-26': { year: 1953, event: "Salk Polio Vaccine Announced", desc: "Dr. Jonas Salk announces he has successfully tested a vaccine against poliomyelitis." },
    '3-27': { year: 1977, event: "Tenerife Airport Disaster", desc: "Two Boeing 747s collide on a runway in Tenerife, Canary Islands, resulting in 583 fatalities, the deadliest accident in aviation history." },
    '3-28': { year: 1979, event: "Three Mile Island Accident", desc: "A partial nuclear meltdown occurs at the Three Mile Island Nuclear Generating Station in Pennsylvania." },
    '3-29': { year: 1974, event: "Terracotta Army Discovered", desc: "Farmers digging a well in Xi'an, China, discover the Terracotta Army, a collection of sculptures depicting the armies of Qin Shi Huang." },
    '3-30': { year: 1867, event: "U.S. Purchases Alaska", desc: "The United States agrees to purchase Alaska from Russia for $7.2 million." },
    '3-31': { year: 1889, event: "Eiffel Tower Opens", desc: "The Eiffel Tower is opened to the public in Paris for the Exposition Universelle." },
    // April
    '4-1': { year: 1976, event: "Apple Inc. Founded", desc: "Steve Jobs, Steve Wozniak, and Ronald Wayne founded Apple Computer, Inc." },
    '4-2': { year: 1982, event: "Falklands War Begins", desc: "Argentina invades the Falkland Islands, starting the Falklands War with the United Kingdom." },
    '4-3': { year: 1973, event: "First Mobile Phone Call", desc: "Motorola employee Martin Cooper makes the first public mobile phone call." },
    '4-4': { year: 1968, event: "Assassination of Martin Luther King Jr.", desc: "Civil rights leader Martin Luther King Jr. is assassinated in Memphis, Tennessee." },
    '4-5': { year: 1994, event: "Kurt Cobain's Death", desc: "Kurt Cobain, frontman of Nirvana, dies by suicide at his home in Seattle." },
    '4-6': { year: 1896, event: "First Modern Olympics", desc: "The first modern Olympic Games open in Athens, Greece." },
    '4-7': { year: 1948, event: "World Health Organization Formed", desc: "The World Health Organization (WHO) is established by the United Nations." },
    '4-8': { year: 1974, event: "Hank Aaron's 715th Home Run", desc: "Hank Aaron hits his 715th career home run, breaking Babe Ruth's long-standing record." },
    '4-9': { year: 1865, event: "End of American Civil War", desc: "Confederate General Robert E. Lee surrenders to Union General Ulysses S. Grant at Appomattox Court House." },
    '4-10': { year: 1970, event: "The Beatles Break Up", desc: "Paul McCartney announces he is leaving The Beatles, effectively dissolving the band." },
    '4-11': { year: 1970, event: "Apollo 13 Launched", desc: "Apollo 13, the seventh crewed mission in the Apollo program, is launched. It would later suffer a critical in-flight emergency." },
    '4-12': { year: 1961, event: "First Human in Space", desc: "Soviet cosmonaut Yuri Gagarin becomes the first human to travel into outer space." },
    '4-13': { year: 1997, event: "Tiger Woods Wins First Masters", desc: "Tiger Woods, age 21, becomes the youngest golfer to win the Masters Tournament." },
    '4-14': { year: 1865, event: "Assassination of Abraham Lincoln", desc: "U.S. President Abraham Lincoln is shot by John Wilkes Booth at Ford's Theatre in Washington, D.C." },
    '4-15': { year: 1912, event: "Sinking of the Titanic", desc: "The RMS Titanic sank in the North Atlantic Ocean after striking an iceberg." },
    '4-16': { year: 1947, event: "Texas City Disaster", desc: "A ship carrying ammonium nitrate explodes in the port of Texas City, causing one of the largest non-nuclear explosions in U.S. history." },
    '4-17': { year: 1961, event: "Bay of Pigs Invasion", desc: "A CIA-sponsored force of Cuban exiles attempts to invade Cuba at the Bay of Pigs." },
    '4-18': { year: 1906, event: "San Francisco Earthquake", desc: "A major earthquake and subsequent fires devastate San Francisco, California." },
    '4-19': { year: 1775, event: "American Revolution Begins", desc: "The 'shot heard 'round the world' is fired at the Battles of Lexington and Concord, starting the American Revolutionary War." },
    '4-20': { year: 1999, event: "Columbine High School Massacre", desc: "Two students carry out a school shooting at Columbine High School in Colorado." },
    '4-21': { year: '753 BC', event: "Founding of Rome", desc: "According to tradition, the city of Rome is founded by Romulus." },
    '4-22': { year: 1970, event: "First Earth Day", desc: "The first Earth Day is celebrated in the United States, marking the birth of the modern environmental movement." },
    '4-23': { year: 1616, event: "Death of William Shakespeare", desc: "English playwright and poet William Shakespeare dies on what is traditionally believed to be his 52nd birthday." },
    '4-24': { year: 1990, event: "Hubble Space Telescope Deployed", desc: "The Hubble Space Telescope is deployed into orbit from the Space Shuttle Discovery." },
    '4-25': { year: 1953, event: "Discovery of DNA Structure", desc: "James Watson and Francis Crick publish their discovery of the double helix structure of DNA." },
    '4-26': { year: 1986, event: "Chernobyl Disaster", desc: "The Chernobyl nuclear power plant in Ukraine suffers a catastrophic explosion, the worst nuclear disaster in history." },
    '4-27': { year: 1994, event: "First All-Race Elections in South Africa", desc: "South Africa holds its first multiracial democratic elections, in which Nelson Mandela is elected president." },
    '4-28': { year: 1945, event: "Execution of Mussolini", desc: "Italian dictator Benito Mussolini is executed by partisans." },
    '4-29': { year: 1992, event: "Los Angeles Riots", desc: "Riots erupt in Los Angeles following the acquittal of police officers in the Rodney King beating case." },
    '4-30': { year: 1945, event: "Adolf Hitler's Death", desc: "Adolf Hitler died by suicide in his bunker in Berlin as Allied forces closed in." },
    // May
    '5-1': { year: 1931, event: "Empire State Building Opens", desc: "The Empire State Building in New York City is officially opened." },
    '5-2': { year: 2011, event: "Osama bin Laden Killed", desc: "U.S. Navy SEALs kill al-Qaeda leader Osama bin Laden in a raid on his compound in Pakistan." },
    '5-3': { year: 1979, event: "Margaret Thatcher Becomes PM", desc: "Margaret Thatcher becomes the first female Prime Minister of the United Kingdom." },
    '5-4': { year: 1970, event: "Kent State Shootings", desc: "Ohio National Guardsmen open fire on students protesting the Vietnam War at Kent State University, killing four." },
    '5-5': { year: 1821, event: "Death of Napoleon Bonaparte", desc: "Napoleon Bonaparte dies in exile on the island of Saint Helena." },
    '5-6': { year: 1937, event: "Hindenburg Disaster", desc: "The German airship Hindenburg bursts into flames while attempting to land in New Jersey." },
    '5-7': { year: 1915, event: "Sinking of the Lusitania", desc: "A German U-boat sinks the British passenger liner RMS Lusitania, a key event leading to U.S. entry into WWI." },
    '5-8': { year: 1945, event: "Victory in Europe Day (V-E Day)", desc: "The formal acceptance by the Allies of Nazi Germany's unconditional surrender, marking the end of WWII in Europe." },
    '5-9': { year: 1960, event: "FDA Approves Birth Control Pill", desc: "The U.S. Food and Drug Administration approves the first commercially produced birth control pill." },
    '5-10': { year: 1869, event: "Transcontinental Railroad Completed", desc: "The First Transcontinental Railroad in the United States is completed with the driving of the 'Golden Spike' at Promontory, Utah." },
    '5-11': { year: 330, event: "Founding of Constantinople", desc: "Emperor Constantine the Great dedicates Constantinople as the new capital of the Roman Empire." },
    '5-12': { year: 1932, event: "Lindbergh Baby Kidnapping", desc: "The body of the kidnapped son of aviator Charles Lindbergh is found." },
    '5-13': { year: 1981, event: "Assassination Attempt on Pope John Paul II", desc: "Pope John Paul II is shot and seriously wounded in St. Peter's Square, Vatican City." },
    '5-14': { year: 1948, event: "Israel Declares Independence", desc: "The state of Israel is proclaimed by David Ben-Gurion." },
    '5-15': { year: 1940, event: "First McDonald's Opens", desc: "Richard and Maurice McDonald open the first McDonald's restaurant in San Bernardino, California." },
    '5-16': { year: 1929, event: "First Academy Awards", desc: "The first Academy Awards ceremony is held in Hollywood, California." },
    '5-17': { year: 1954, event: "Brown v. Board of Education", desc: "The U.S. Supreme Court rules that racial segregation in public schools is unconstitutional." },
    '5-18': { year: 1980, event: "Eruption of Mount St. Helens", desc: "Mount St. Helens in Washington state erupts, causing the deadliest volcanic event in U.S. history." },
    '5-19': { year: 1536, event: "Execution of Anne Boleyn", desc: "Anne Boleyn, the second wife of King Henry VIII of England, is beheaded." },
    '5-20': { year: 1927, event: "Charles Lindbergh's Solo Flight", desc: "Charles Lindbergh takes off from New York for the first solo, non-stop transatlantic flight." },
    '5-21': { year: 1932, event: "Amelia Earhart's Solo Flight", desc: "Amelia Earhart becomes the first woman to fly solo across the Atlantic Ocean." },
    '5-22': { year: 1990, event: "Windows 3.0 Released", desc: "Microsoft releases Windows 3.0, a version that significantly improves the graphical user interface." },
    '5-23': { year: 1934, event: "Bonnie and Clyde Killed", desc: "Infamous outlaws Bonnie Parker and Clyde Barrow are ambushed and killed by law officers in Louisiana." },
    '5-24': { year: 1844, event: "First Telegraph Message", desc: "Samuel Morse sends the first public telegraph message, 'What hath God wrought', from Washington, D.C. to Baltimore." },
    '5-25': { year: 1977, event: "Star Wars Released", desc: "The first film in the Star Wars saga, Episode IV: A New Hope, is released in theaters." },
    '5-26': { year: 1897, event: "Dracula Published", desc: "Bram Stoker's gothic horror novel 'Dracula' is published." },
    '5-27': { year: 1937, event: "Golden Gate Bridge Opens", desc: "The Golden Gate Bridge opens to pedestrian traffic, connecting San Francisco and Marin County." },
    '5-28': { year: 1961, event: "Amnesty International Founded", desc: "British lawyer Peter Benenson founds the human rights organization Amnesty International." },
    '5-29': { year: 1953, event: "First Ascent of Mount Everest", desc: "Edmund Hillary and Tenzing Norgay became the first climbers confirmed to have reached the summit of Mount Everest." },
    '5-30': { year: 1431, event: "Joan of Arc Burned at the Stake", desc: "Joan of Arc is executed by burning at the stake in Rouen, France." },
    '5-31': { year: 1911, event: "RMS Titanic Launched", desc: "The hull of the RMS Titanic is launched at Belfast, Northern Ireland." },
    // June
    '6-1': { year: 1980, event: "CNN Launches", desc: "The Cable News Network (CNN), the world's first 24-hour television news channel, is launched." },
    '6-2': { year: 1953, event: "Coronation of Queen Elizabeth II", desc: "The coronation of Queen Elizabeth II takes place at Westminster Abbey in London." },
    '6-3': { year: 1965, event: "First American Spacewalk", desc: "Astronaut Ed White performs the first spacewalk by an American, during the Gemini 4 mission." },
    '6-4': { year: 1989, event: "Tiananmen Square Massacre", desc: "Chinese troops violently suppress pro-democracy protests in and around Tiananmen Square in Beijing." },
    '6-5': { year: 1968, event: "Assassination of Robert F. Kennedy", desc: "U.S. Senator and presidential candidate Robert F. Kennedy is shot at the Ambassador Hotel in Los Angeles." },
    '6-6': { year: 1944, event: "D-Day", desc: "The Allied forces launched the largest amphibious invasion in history, landing on the beaches of Normandy, France." },
    '6-7': { year: 1975, event: "Sony Introduces Betamax", desc: "Sony introduces the Betamax videocassette recorder for home use." },
    '6-8': { year: 1949, event: "'Nineteen Eighty-Four' Published", desc: "George Orwell's dystopian novel 'Nineteen Eighty-Four' is published." },
    '6-9': { year: 1973, event: "Secretariat Wins Triple Crown", desc: "The racehorse Secretariat wins the Belmont Stakes by a record 31 lengths to capture the Triple Crown." },
    '6-10': { year: 1692, event: "First Salem Witch Trials Execution", desc: "Bridget Bishop becomes the first person to be executed for witchcraft during the Salem witch trials." },
    '6-11': { year: 1963, event: "Stand in the Schoolhouse Door", desc: "Alabama Governor George Wallace stands at the door of the University of Alabama to block two black students from enrolling." },
    '6-12': { year: 1987, event: "'Tear Down This Wall!' Speech", desc: "U.S. President Ronald Reagan challenges Soviet leader Mikhail Gorbachev to 'tear down this wall' in a speech at the Brandenburg Gate in Berlin." },
    '6-13': { year: 1966, event: "Miranda v. Arizona Decision", desc: "The U.S. Supreme Court rules in Miranda v. Arizona that criminal suspects must be informed of their constitutional rights before questioning." },
    '6-14': { year: 1777, event: "U.S. Flag Adopted", desc: "The Continental Congress adopts the 'Stars and Stripes' as the national flag of the United States." },
    '6-15': { year: 1215, event: "Magna Carta Sealed", desc: "King John of England seals the Magna Carta, a charter of rights that would influence constitutional law for centuries." },
    '6-16': { year: 1963, event: "First Woman in Space", desc: "Soviet cosmonaut Valentina Tereshkova becomes the first woman to travel into space." },
    '6-17': { year: 1972, event: "Watergate Break-in", desc: "Five men are arrested for breaking into the Democratic National Committee headquarters at the Watergate complex in Washington, D.C." },
    '6-18': { year: 1815, event: "Battle of Waterloo", desc: "Napoleon Bonaparte is defeated by a coalition of forces led by the Duke of Wellington and Gebhard von Blücher, ending his rule as Emperor of the French." },
    '6-19': { year: 1865, event: "Juneteenth", desc: "Union soldiers bring the news of freedom to enslaved African Americans in Galveston, Texas, two and a half years after the Emancipation Proclamation." },
    '6-20': { year: 1975, event: "'Jaws' Released", desc: "Steven Spielberg's film 'Jaws' is released, becoming the first summer blockbuster." },
    '6-21': { year: 1982, event: "John Hinckley Jr. Verdict", desc: "John Hinckley Jr. is found not guilty by reason of insanity for the assassination attempt on U.S. President Ronald Reagan." },
    '6-22': { year: 1941, event: "Operation Barbarossa", desc: "Nazi Germany invades the Soviet Union in the largest German military operation of World War II." },
    '6-23': { year: 2016, event: "Brexit Referendum", desc: "The United Kingdom votes in a referendum to leave the European Union." },
    '6-24': { year: 1948, event: "Berlin Blockade Begins", desc: "The Soviet Union begins the Berlin Blockade, cutting off all land and rail access to West Berlin." },
    '6-25': { year: 1950, event: "Korean War Begins", desc: "The Korean War begins as North Korean forces cross the 38th parallel and invade South Korea." },
    '6-26': { year: 1945, event: "UN Charter Signed", desc: "The United Nations Charter is signed in San Francisco by 50 original member countries." },
    '6-27': { year: 1976, event: "Entebbe Hostage Crisis", desc: "An Air France flight is hijacked by terrorists and flown to Entebbe, Uganda, leading to a dramatic rescue operation." },
    '6-28': { year: 1914, event: "Assassination of Archduke Franz Ferdinand", desc: "The assassination of Archduke Franz Ferdinand of Austria in Sarajevo is the catalyst for World War I." },
    '6-29': { year: 2007, event: "First iPhone Goes on Sale", desc: "Apple releases the first iPhone, revolutionizing the mobile phone industry." },
    '6-30': { year: 1908, event: "Tunguska Event", desc: "A massive explosion, believed to be caused by an air burst of a meteoroid, flattens an estimated 80 million trees over a vast area in Siberia." },
    // July
    '7-1': { year: 1916, event: "Battle of the Somme Begins", desc: "One of the bloodiest battles in human history begins, with over 1.3 million casualties." },
    '7-2': { year: 1964, event: "Civil Rights Act Signed", desc: "U.S. President Lyndon B. Johnson signs the Civil Rights Act of 1964 into law, outlawing discrimination based on race, color, religion, sex, or national origin." },
    '7-3': { year: 1863, event: "Battle of Gettysburg Ends", desc: "The Battle of Gettysburg, a turning point in the American Civil War, ends with a Union victory." },
    '7-4': { year: 1776, event: "U.S. Declaration of Independence", desc: "The Continental Congress adopted the Declaration of Independence, separating the 13 colonies from Great Britain." },
    '7-5': { year: 1996, event: "Dolly the Sheep Born", desc: "Dolly, the first mammal to be successfully cloned from an adult cell, is born in Scotland." },
    '7-6': { year: 1885, event: "Pasteur Tests Rabies Vaccine", desc: "Louis Pasteur successfully tests his rabies vaccine on a young boy who had been bitten by a rabid dog." },
    '7-7': { year: 2005, event: "London Bombings (7/7)", desc: "A series of coordinated suicide bomb attacks target London's public transport system during the morning rush hour." },
    '7-8': { year: 1497, event: "Vasco da Gama's Voyage", desc: "Portuguese explorer Vasco da Gama departs from Lisbon on his first voyage to find a maritime route to India." },
    '7-9': { year: 1868, event: "14th Amendment Ratified", desc: "The 14th Amendment to the U.S. Constitution, granting citizenship to all persons born or naturalized in the U.S., is ratified." },
    '7-10': { year: 1940, event: "Battle of Britain Begins", desc: "The Battle of Britain, a major air campaign fought between the Royal Air Force and the Luftwaffe, begins." },
    '7-11': { year: 1987, event: "Day of Five Billion", desc: "The world's population is estimated to have reached five billion people." },
    '7-12': { year: 1962, event: "The Rolling Stones' First Concert", desc: "The Rolling Stones perform their first-ever concert at the Marquee Club in London." },
    '7-13': { year: 1985, event: "Live Aid Concerts", desc: "The Live Aid benefit concerts, organized to raise funds for famine relief in Ethiopia, are held in London and Philadelphia." },
    '7-14': { year: 1789, event: "Storming of the Bastille", desc: "Revolutionaries in Paris storm the Bastille fortress, a pivotal event in the French Revolution." },
    '7-15': { year: 1799, event: "Rosetta Stone Found", desc: "The Rosetta Stone, a key to deciphering ancient Egyptian hieroglyphs, is discovered in Egypt by a French soldier." },
    '7-16': { year: 1945, event: "Trinity Test", desc: "The United States Army conducts the first detonation of a nuclear weapon, the Trinity test, in New Mexico." },
    '7-17': { year: 1955, event: "Disneyland Opens", desc: "Disneyland, the first theme park of its kind, opens in Anaheim, California." },
    '7-18': { year: 1918, event: "Nelson Mandela Born", desc: "Nelson Mandela, anti-apartheid revolutionary and future President of South Africa, is born." },
    '7-19': { year: 1848, event: "Seneca Falls Convention", desc: "The first women's rights convention in the United States, the Seneca Falls Convention, begins in New York." },
    '7-20': { year: 1969, event: "Apollo 11 Moon Landing", desc: "Neil Armstrong and Buzz Aldrin became the first humans to land on the Moon." },
    '7-21': { year: 1861, event: "First Battle of Bull Run", desc: "The First Battle of Bull Run, the first major battle of the American Civil War, is fought." },
    '7-22': { year: 1934, event: "John Dillinger Killed", desc: "Infamous American bank robber John Dillinger is shot and killed by FBI agents in Chicago." },
    '7-23': { year: 1903, event: "First Ford Car Sold", desc: "The Ford Motor Company sells its first car, the Model A." },
    '7-24': { year: 1911, event: "Machu Picchu Rediscovered", desc: "American historian Hiram Bingham is led to the Inca site of Machu Picchu, bringing it to international attention." },
    '7-25': { year: 1978, event: "First 'Test-Tube Baby' Born", desc: "Louise Brown, the world's first baby to be conceived through in vitro fertilisation (IVF), is born in England." },
    '7-26': { year: 1990, event: "Americans with Disabilities Act", desc: "The Americans with Disabilities Act (ADA) is signed into law in the United States." },
    '7-27': { year: 1953, event: "Korean War Armistice", desc: "The Korean War Armistice Agreement is signed, ending the hostilities of the Korean War." },
    '7-28': { year: 1914, event: "World War I Begins", desc: "Austria-Hungary declares war on Serbia, officially beginning World War I." },
    '7-29': { year: 1958, event: "NASA Created", desc: "The National Aeronautics and Space Administration (NASA) is founded in the United States." },
    '7-30': { year: 1965, event: "Medicare and Medicaid Signed", desc: "U.S. President Lyndon B. Johnson signs the Social Security Act of 1965, establishing Medicare and Medicaid." },
    '7-31': { year: 1971, event: "Apollo 15's Lunar Rover", desc: "Apollo 15 astronauts David Scott and James Irwin become the first humans to drive a rover on the Moon." },
    // August
    '8-1': { year: 1981, event: "MTV Launches", desc: "MTV (Music Television) is launched, with its first music video being 'Video Killed the Radio Star' by The Buggles." },
    '8-2': { year: 1990, event: "Iraq Invades Kuwait", desc: "Iraq invades its neighboring country Kuwait, leading to the start of the Gulf War." },
    '8-3': { year: 1492, event: "Columbus Sets Sail", desc: "Christopher Columbus departs from Palos de la Frontera, Spain, on his first voyage across the Atlantic Ocean." },
    '8-4': { year: 1944, event: "Anne Frank Arrested", desc: "Anne Frank and her family are arrested by the Gestapo in Amsterdam after being in hiding for two years." },
    '8-5': { year: 1963, event: "Partial Nuclear Test Ban Treaty", desc: "The United States, the United Kingdom, and the Soviet Union sign the Partial Nuclear Test Ban Treaty, prohibiting nuclear weapons tests in the atmosphere, outer space, and under water." },
    '8-6': { year: 1945, event: "Atomic Bombing of Hiroshima", desc: "The United States drops an atomic bomb on the city of Hiroshima, Japan, during World War II." },
    '8-7': { year: 1998, event: "U.S. Embassy Bombings", desc: "Al-Qaeda carries out truck bomb attacks on U.S. embassies in Kenya and Tanzania, killing 224 people." },
    '8-8': { year: 1974, event: "Nixon Announces Resignation", desc: "U.S. President Richard Nixon announces his resignation from office in a televised address, effective the next day, due to the Watergate scandal." },
    '8-9': { year: 1945, event: "Atomic Bombing of Nagasaki", desc: "The United States drops a second atomic bomb on the city of Nagasaki, Japan." },
    '8-10': { year: 1792, event: "Storming of the Tuileries", desc: "During the French Revolution, revolutionaries storm the Tuileries Palace, leading to the arrest of King Louis XVI." },
    '8-11': { year: 1965, event: "Watts Riots Begin", desc: "A major race riot begins in the Watts neighborhood of Los Angeles, lasting for six days." },
    '8-12': { year: 1981, event: "IBM PC Introduced", desc: "IBM introduces its first Personal Computer, the IBM PC model 5150." },
    '8-13': { year: 1961, event: "Berlin Wall Construction Begins", desc: "East Germany begins construction of the Berlin Wall to prevent its citizens from fleeing to the West." },
    '8-14': { year: 1945, event: "V-J Day Announced", desc: "U.S. President Harry S. Truman announces the unconditional surrender of Japan, marking the end of World War II (Victory over Japan Day)." },
    '8-15': { year: 1969, event: "Woodstock Festival Begins", desc: "The Woodstock Music & Art Fair opens in Bethel, New York, becoming a defining moment for the 1960s counterculture." },
    '8-16': { year: 1977, event: "Death of Elvis Presley", desc: "Rock and roll legend Elvis Presley dies at his Graceland home in Memphis, Tennessee." },
    '8-17': { year: 1945, event: "Indonesian Independence", desc: "Indonesia declares its independence from Dutch rule." },
    '8-18': { year: 1920, event: "19th Amendment Ratified", desc: "The 19th Amendment to the U.S. Constitution, granting women the right to vote, is ratified." },
    '8-19': { year: 1934, event: "Hitler Becomes Führer", desc: "Adolf Hitler becomes the Führer of Germany after the death of President Paul von Hindenburg." },
    '8-20': { year: 1968, event: "Prague Spring Crushed", desc: "The Soviet Union and Warsaw Pact allies invade Czechoslovakia to crush the Prague Spring reform movement." },
    '8-21': { year: 1959, event: "Hawaii Becomes 50th State", desc: "Hawaii is admitted as the 50th U.S. state." },
    '8-22': { year: 1485, event: "Battle of Bosworth Field", desc: "The Battle of Bosworth Field ends the Wars of the Roses in England, with Henry Tudor defeating King Richard III." },
    '8-23': { year: 1939, event: "Molotov–Ribbentrop Pact", desc: "Nazi Germany and the Soviet Union sign a non-aggression pact, which includes a secret protocol to divide Eastern Europe between them." },
    '8-24': { year: 79, event: "Eruption of Mount Vesuvius", desc: "Mount Vesuvius erupts, burying the Roman cities of Pompeii and Herculaneum in ash and pumice." },
    '8-25': { year: 1944, event: "Liberation of Paris", desc: "Allied forces liberate Paris from German occupation during World War II." },
    '8-26': { year: 1883, event: "Krakatoa Eruption", desc: "The volcanic island of Krakatoa begins its cataclysmic eruption, one of the deadliest and most destructive in recorded history." },
    '8-27': { year: 1955, event: "Guinness Book of Records", desc: "The first edition of the Guinness Book of Records is published." },
    '8-28': { year: 1963, event: "'I Have a Dream' Speech", desc: "Martin Luther King Jr. delivers his famous 'I Have a Dream' speech during the March on Washington for Jobs and Freedom." },
    '8-29': { year: 2005, event: "Hurricane Katrina", desc: "Hurricane Katrina makes landfall as a Category 3 storm, devastating the U.S. Gulf Coast, particularly New Orleans." },
    '8-30': { year: 1967, event: "Thurgood Marshall Confirmed", desc: "Thurgood Marshall is confirmed as the first African American Justice of the U.S. Supreme Court." },
    '8-31': { year: 1997, event: "Death of Princess Diana", desc: "Diana, Princess of Wales, dies in a car crash in Paris." },
    // September
    '9-1': { year: 1939, event: "Start of World War II", desc: "Germany invades Poland, leading the United Kingdom and France to declare war, beginning WWII in Europe." },
    '9-2': { year: 1945, event: "End of World War II (V-J Day)", desc: "Japan formally surrenders aboard the USS Missouri, officially ending World War II." },
    '9-3': { year: 1783, event: "Treaty of Paris Signed", desc: "The Treaty of Paris is signed by representatives of Great Britain and the United States, officially ending the American Revolutionary War." },
    '9-4': { year: 1998, event: "Google Founded", desc: "Google is founded by Larry Page and Sergey Brin while they are Ph.D. students at Stanford University." },
    '9-5': { year: 1972, event: "Munich Massacre", desc: "The Munich massacre begins at the 1972 Summer Olympics when members of the Palestinian terrorist group Black September take Israeli athletes hostage." },
    '9-6': { year: 1997, event: "Funeral of Princess Diana", desc: "The funeral of Diana, Princess of Wales, is held in London, watched by an estimated 2.5 billion people worldwide." },
    '9-7': { year: 1822, event: "Brazilian Independence", desc: "Brazil declares its independence from Portugal." },
    '9-8': { year: 1974, event: "Nixon Pardon", desc: "U.S. President Gerald Ford grants a full and unconditional pardon to Richard Nixon for any crimes he might have committed against the United States as president." },
    '9-9': { year: 1976, event: "Death of Mao Zedong", desc: "Mao Zedong, the founding father of the People's Republic of China, dies." },
    '9-10': { year: 2008, event: "Large Hadron Collider Powered On", desc: "The Large Hadron Collider, the world's largest and most powerful particle accelerator, is first powered on." },
    '9-11': { year: 2001, event: "September 11th Attacks", desc: "A series of coordinated terrorist attacks by al-Qaeda against the United States." },
    '9-12': { year: 1953, event: "JFK Marries Jacqueline Bouvier", desc: "U.S. Senator and future President John F. Kennedy marries Jacqueline Bouvier." },
    '9-13': { year: 1993, event: "Oslo I Accord Signed", desc: "Israeli Prime Minister Yitzhak Rabin and PLO Chairman Yasser Arafat sign the Oslo I Accord at the White House." },
    '9-14': { year: 1814, event: "The Star-Spangled Banner", desc: "Francis Scott Key writes the poem 'Defence of Fort M'Henry', which is later set to music and becomes the U.S. national anthem." },
    '9-15': { year: 2008, event: "Lehman Brothers Bankruptcy", desc: "Lehman Brothers files for Chapter 11 bankruptcy, a pivotal event in the 2008 global financial crisis." },
    '9-16': { year: 1620, event: "Mayflower Departs", desc: "The Mayflower ship departs from Plymouth, England, carrying the Pilgrims to North America." },
    '9-17': { year: 1787, event: "U.S. Constitution Signed", desc: "The United States Constitution is signed by delegates to the Constitutional Convention in Philadelphia." },
    '9-18': { year: 1947, event: "U.S. Air Force Established", desc: "The United States Air Force is established as a separate branch of the U.S. military." },
    '9-19': { year: 1893, event: "New Zealand Women's Suffrage", desc: "New Zealand becomes the first self-governing country in the world in which all women have the right to vote in parliamentary elections." },
    '9-20': { year: 1519, event: "Magellan's Circumnavigation", desc: "Ferdinand Magellan sets sail from Spain with a fleet of five ships on a voyage to circumnavigate the globe." },
    '9-21': { year: 1981, event: "Sandra Day O'Connor Confirmed", desc: "Sandra Day O'Connor is unanimously approved by the U.S. Senate, becoming the first female Supreme Court justice." },
    '9-22': { year: 1862, event: "Preliminary Emancipation Proclamation", desc: "U.S. President Abraham Lincoln issues the preliminary Emancipation Proclamation, declaring that all slaves in Confederate-held territory would be freed." },
    '9-23': { year: 1846, event: "Discovery of Neptune", desc: "The planet Neptune is discovered by astronomers Urbain Le Verrier, John Couch Adams, and Johann Galle." },
    '9-24': { year: 1957, event: "Little Rock Nine", desc: "Under escort from the 101st Airborne Division, nine African-American students are allowed into Central High School in Little Rock, Arkansas." },
    '9-25': { year: 1789, event: "Bill of Rights Proposed", desc: "The U.S. Congress proposes the Bill of Rights, the first ten amendments to the Constitution." },
    '9-26': { year: 1960, event: "First Televised Presidential Debate", desc: "The first-ever televised U.S. presidential debate takes place between John F. Kennedy and Richard Nixon." },
    '9-27': { year: 1905, event: "Einstein's E=mc² Paper", desc: "Albert Einstein publishes his paper 'Does the Inertia of a Body Depend Upon Its Energy Content?', introducing the concept of mass-energy equivalence." },
    '9-28': { year: 1066, event: "Norman Conquest Begins", desc: "William the Conqueror invades England, beginning the Norman Conquest." },
    '9-29': { year: 1982, event: "Chicago Tylenol Murders", desc: "The first of several deaths occurs from cyanide-laced Tylenol capsules in the Chicago area, leading to major reforms in packaging for over-the-counter drugs." },
    '9-30': { year: 1955, event: "Death of James Dean", desc: "Actor James Dean dies in a car crash at the age of 24." },
    // October
    '10-1': { year: 1949, event: "People's Republic of China Proclaimed", desc: "Mao Zedong proclaims the establishment of the People's Republic of China." },
    '10-2': { year: 1967, event: "Thurgood Marshall Sworn In", desc: "Thurgood Marshall is sworn in as the first African American justice of the U.S. Supreme Court." },
    '10-3': { year: 1990, event: "German Reunification", desc: "The German Democratic Republic (East Germany) is dissolved and its territory becomes part of the Federal Republic of Germany (West Germany), officially reunifying Germany." },
    '10-4': { year: 1957, event: "Sputnik 1 Launch", desc: "The Soviet Union launches Sputnik 1, the first artificial Earth satellite, starting the Space Race." },
    '10-5': { year: 1962, event: "The Beatles Release 'Love Me Do'", desc: "The Beatles release their first single, 'Love Me Do', in the United Kingdom." },
    '10-6': { year: 1973, event: "Yom Kippur War Begins", desc: "A coalition of Arab states led by Egypt and Syria launches a surprise attack on Israel on Yom Kippur, the holiest day in Judaism." },
    '10-7': { year: 2001, event: "War in Afghanistan Begins", desc: "The United States and its allies begin the invasion of Afghanistan in response to the September 11th attacks." },
    '10-8': { year: 1871, event: "Great Chicago Fire", desc: "The Great Chicago Fire begins, burning for three days and destroying a large portion of the city." },
    '10-9': { year: 1967, event: "Che Guevara Executed", desc: "Marxist revolutionary Che Guevara is executed in Bolivia." },
    '10-10': { year: 1973, event: "Spiro Agnew Resigns", desc: "U.S. Vice President Spiro Agnew resigns amid a corruption scandal." },
    '10-11': { year: 1986, event: "Reykjavík Summit", desc: "U.S. President Ronald Reagan and Soviet leader Mikhail Gorbachev meet in Reykjavík, Iceland, for a summit that paves the way for a major nuclear arms treaty." },
    '10-12': { year: 1492, event: "Columbus Reaches the Americas", desc: "Christopher Columbus makes his first landfall in the Americas, on an island in the Bahamas." },
    '10-13': { year: 1307, event: "Arrest of the Knights Templar", desc: "King Philip IV of France orders the simultaneous arrest of hundreds of Knights Templar." },
    '10-14': { year: 1066, event: "Battle of Hastings", desc: "The Norman army of William the Conqueror defeats the English forces of King Harold Godwinson, a pivotal event in the Norman conquest of England." },
    '10-15': { year: 1962, event: "Cuban Missile Crisis Begins", desc: "The Cuban Missile Crisis begins as U.S. reconnaissance photos reveal Soviet nuclear missile sites in Cuba." },
    '10-16': { year: 1968, event: "Black Power Salute", desc: "African American athletes Tommie Smith and John Carlos raise their fists in a Black Power salute during a medal ceremony at the Summer Olympics in Mexico City." },
    '10-17': { year: 1931, event: "Al Capone Convicted", desc: "Gangster Al Capone is convicted of income tax evasion." },
    '10-18': { year: 1867, event: "U.S. Takes Possession of Alaska", desc: "The United States formally takes possession of Alaska from Russia after purchasing the territory." },
    '10-19': { year: 1781, event: "Surrender at Yorktown", desc: "British General Charles Cornwallis surrenders to General George Washington at Yorktown, Virginia, effectively ending the American Revolutionary War." },
    '10-20': { year: 1973, event: "Sydney Opera House Opens", desc: "The Sydney Opera House is opened by Queen Elizabeth II." },
    '10-21': { year: 1805, event: "Battle of Trafalgar", desc: "The British Royal Navy, led by Admiral Horatio Nelson, decisively defeats the combined fleets of the French and Spanish Navies." },
    '10-22': { year: 1962, event: "Kennedy Announces Cuban Blockade", desc: "U.S. President John F. Kennedy announces a naval 'quarantine' of Cuba in response to the discovery of Soviet missiles." },
    '10-23': { year: 1956, event: "Hungarian Revolution Begins", desc: "A nationwide revolt against the Soviet-backed government of Hungary begins." },
    '10-24': { year: 1929, event: "Black Thursday", desc: "The Wall Street Crash of 1929 begins on 'Black Thursday', a key event leading to the Great Depression." },
    '10-25': { year: 1415, event: "Battle of Agincourt", desc: "The English army, led by King Henry V, achieves a major victory against a larger French army in the Hundred Years' War." },
    '10-26': { year: 1881, event: "Gunfight at the O.K. Corral", desc: "A famous shootout occurs between lawmen and a group of outlaws in Tombstone, Arizona." },
    '10-27': { year: 1904, event: "New York City Subway Opens", desc: "The first underground line of the New York City Subway opens to the public." },
    '10-28': { year: 1886, event: "Statue of Liberty Dedicated", desc: "The Statue of Liberty is formally dedicated in New York Harbor." },
    '10-29': { year: 1929, event: "Black Tuesday", desc: "The Wall Street Crash of 1929 worsens on 'Black Tuesday' as stock prices collapse completely." },
    '10-30': { year: 1938, event: "War of the Worlds Broadcast", desc: "Orson Welles's radio adaptation of 'The War of the Worlds' is broadcast, causing panic among some listeners who believed it to be a real news report." },
    '10-31': { year: 1517, event: "The Ninety-five Theses", desc: "Martin Luther posts his Ninety-five Theses on the door of All Saints' Church in Wittenberg, sparking the Protestant Reformation." },
    // November
    '11-1': { year: 1952, event: "First Hydrogen Bomb Test", desc: "The United States successfully detonates the first hydrogen bomb, codenamed 'Ivy Mike', at Enewetak Atoll." },
    '11-2': { year: 1983, event: "Martin Luther King Jr. Day", desc: "U.S. President Ronald Reagan signs a bill creating a federal holiday to honor Martin Luther King Jr." },
    '11-3': { year: 1957, event: "Laika in Space", desc: "The Soviet Union launches Sputnik 2, carrying the dog Laika, the first living animal to orbit the Earth." },
    '11-4': { year: 1979, event: "Iran Hostage Crisis Begins", desc: "Iranian students seize the U.S. Embassy in Tehran, taking 52 American diplomats and citizens hostage for 444 days." },
    '11-5': { year: 1605, event: "Gunpowder Plot", desc: "Guy Fawkes is arrested for his role in the Gunpowder Plot to blow up the English Parliament." },
    '11-6': { year: 1860, event: "Abraham Lincoln Elected President", desc: "Abraham Lincoln is elected as the 16th President of the United States, leading to the secession of Southern states." },
    '11-7': { year: 1917, event: "October Revolution", desc: "The Bolsheviks, led by Vladimir Lenin, storm the Winter Palace and seize power in Russia." },
    '11-8': { year: 1895, event: "Discovery of X-rays", desc: "Wilhelm Röntgen discovers X-rays, a landmark achievement in physics and medicine." },
    '11-9': { year: 1989, event: "Fall of the Berlin Wall", desc: "The Berlin Wall, which had divided East and West Berlin, is opened, leading to German reunification." },
    '11-10': { year: 1975, event: "Sinking of the SS Edmund Fitzgerald", desc: "The freighter SS Edmund Fitzgerald sinks in Lake Superior during a storm, killing all 29 crew members." },
    '11-11': { year: 1918, event: "End of World War I", desc: "An armistice is signed between the Allies and Germany, ending the fighting in World War I." },
    '11-12': { year: 1980, event: "Voyager 1 Reaches Saturn", desc: "NASA's Voyager 1 space probe makes its closest approach to Saturn, providing detailed images of the planet and its rings." },
    '11-13': { year: 1982, event: "Vietnam Veterans Memorial Dedicated", desc: "The Vietnam Veterans Memorial is dedicated in Washington, D.C." },
    '11-14': { year: 1969, event: "Apollo 12 Launches", desc: "NASA launches Apollo 12, the second crewed mission to land on the Moon." },
    '11-15': { year: 1864, event: "Sherman's March to the Sea", desc: "Union General William T. Sherman begins his 'March to the Sea' during the American Civil War." },
    '11-16': { year: 1945, event: "UNESCO Founded", desc: "The United Nations Educational, Scientific and Cultural Organization (UNESCO) is founded." },
    '11-17': { year: 1869, event: "Suez Canal Opens", desc: "The Suez Canal, connecting the Mediterranean Sea to the Red Sea, is inaugurated in Egypt." },
    '11-18': { year: 1928, event: "Mickey Mouse Debuts", desc: "Mickey Mouse makes his first appearance in the animated short film 'Steamboat Willie'." },
    '11-19': { year: 1863, event: "Gettysburg Address", desc: "U.S. President Abraham Lincoln delivers the Gettysburg Address at the dedication of the Soldiers' National Cemetery in Gettysburg, Pennsylvania." },
    '11-20': { year: 1945, event: "Nuremberg Trials Begin", desc: "The Nuremberg trials of Nazi war criminals begin." },
    '11-21': { year: 1877, event: "Phonograph Announced", desc: "Thomas Edison announces his invention of the phonograph, a device for recording and replaying sound." },
    '11-22': { year: 1963, event: "JFK Assassination", desc: "U.S. President John F. Kennedy is assassinated in Dallas, Texas." },
    '11-23': { year: 1890, event: "End of Netherlands-Luxembourg Union", desc: "King William III of the Netherlands dies without a male heir, ending the personal union between the Netherlands and Luxembourg." },
    '11-24': { year: 1859, event: "'On the Origin of Species' Published", desc: "Charles Darwin's groundbreaking book 'On the Origin of Species' is published." },
    '11-25': { year: 1915, event: "General Relativity Presented", desc: "Albert Einstein presents his theory of general relativity to the Prussian Academy of Sciences." },
    '11-26': { year: 1922, event: "Tutankhamun's Tomb Entered", desc: "Archaeologist Howard Carter and his sponsor Lord Carnarvon become the first people to enter the tomb of Pharaoh Tutankhamun in over 3,000 years." },
    '11-27': { year: 1095, event: "First Crusade Called", desc: "Pope Urban II calls for the First Crusade at the Council of Clermont." },
    '11-28': { year: 1943, event: "Tehran Conference", desc: "Winston Churchill, Franklin D. Roosevelt, and Joseph Stalin meet at the Tehran Conference to discuss strategy for World War II." },
    '11-29': { year: 1947, event: "UN Partition Plan for Palestine", desc: "The United Nations General Assembly votes to approve the partition plan for Palestine, leading to the creation of the state of Israel." },
    '11-30': { year: 1982, event: "Michael Jackson's 'Thriller' Released", desc: "Michael Jackson's album 'Thriller' is released, going on to become the best-selling album of all time." },
    // December
    '12-1': { year: 1955, event: "Rosa Parks Arrested", desc: "Rosa Parks is arrested for refusing to give up her seat to a white passenger on a bus in Montgomery, Alabama, sparking the Montgomery bus boycott." },
    '12-2': { year: 1804, event: "Napoleon Crowned Emperor", desc: "Napoleon Bonaparte is crowned Emperor of the French at Notre-Dame de Paris." },
    '12-3': { year: 1967, event: "First Human Heart Transplant", desc: "Dr. Christiaan Barnard performs the world's first successful human-to-human heart transplant in Cape Town, South Africa." },
    '12-4': { year: 1996, event: "Mars Pathfinder Launched", desc: "NASA launches the Mars Pathfinder mission, which would successfully land a rover on Mars." },
    '12-5': { year: 1933, event: "Prohibition Ends", desc: "The 21st Amendment to the U.S. Constitution is ratified, repealing the 18th Amendment and ending Prohibition." },
    '12-6': { year: 1865, event: "13th Amendment Ratified", desc: "The 13th Amendment to the U.S. Constitution, abolishing slavery, is ratified." },
    '12-7': { year: 1941, event: "Attack on Pearl Harbor", desc: "The Imperial Japanese Navy launched a surprise military strike against the US naval base at Pearl Harbor, Hawaii." },
    '12-8': { year: 1980, event: "John Lennon Assassinated", desc: "Musician and former member of The Beatles, John Lennon, is assassinated in New York City." },
    '12-9': { year: 1992, event: "Separation of Charles and Diana", desc: "British Prime Minister John Major announces the formal separation of Prince Charles and Princess Diana." },
    '12-10': { year: 1948, event: "Universal Declaration of Human Rights", desc: "The United Nations General Assembly adopts the Universal Declaration of Human Rights." },
    '12-11': { year: 1936, event: "Edward VIII Abdicates", desc: "King Edward VIII of the United Kingdom abdicates the throne to marry American divorcée Wallis Simpson." },
    '12-12': { year: 1913, event: "Mona Lisa Recovered", desc: "Leonardo da Vinci's 'Mona Lisa' is recovered in Florence after being stolen from the Louvre two years earlier." },
    '12-13': { year: 2003, event: "Saddam Hussein Captured", desc: "Former Iraqi President Saddam Hussein is captured by U.S. forces near his hometown of Tikrit." },
    '12-14': { year: 1911, event: "South Pole Reached", desc: "Norwegian explorer Roald Amundsen and his team become the first people to reach the South Pole." },
    '12-15': { year: 1791, event: "Bill of Rights Ratified", desc: "The United States Bill of Rights, the first ten amendments to the Constitution, is ratified." },
    '12-16': { year: 1773, event: "Boston Tea Party", desc: "American colonists, frustrated at Britain for imposing 'taxation without representation,' dumped 342 chests of tea into Boston Harbor." },
    '12-17': { year: 1903, event: "First Powered Flight", desc: "The Wright brothers made the first successful sustained flight of a powered, heavier-than-air aircraft." },
    '12-18': { year: 1892, event: "'The Nutcracker' Premieres", desc: "Pyotr Ilyich Tchaikovsky's ballet 'The Nutcracker' premieres in Saint Petersburg, Russia." },
    '12-19': { year: 1998, event: "Bill Clinton Impeached", desc: "The U.S. House of Representatives impeaches President Bill Clinton for perjury and obstruction of justice." },
    '12-20': { year: 1803, event: "Louisiana Purchase", desc: "The United States formally takes possession of the Louisiana Territory from France." },
    '12-21': { year: 1968, event: "Apollo 8 Launched", desc: "NASA launches Apollo 8, the first crewed spacecraft to orbit the Moon." },
    '12-22': { year: 1882, event: "First Electric Christmas Lights", desc: "Edward Johnson, an associate of Thomas Edison, displays the first Christmas tree illuminated with electric lights." },
    '12-23': { year: 1913, event: "Federal Reserve Created", desc: "The U.S. Congress passes the Federal Reserve Act, creating the central banking system of the United States." },
    '12-24': { year: 1914, event: "Christmas Truce", desc: "An unofficial ceasefire occurs along the Western Front of World War I, during which soldiers from both sides exchange greetings and sing carols." },
    '12-25': { year: 800, event: "Charlemagne Crowned Emperor", desc: "Charlemagne was crowned Emperor of the Romans by Pope Leo III in Rome, uniting much of Western Europe." },
    '12-26': { year: 2004, event: "Indian Ocean Tsunami", desc: "A massive undersea earthquake off the coast of Sumatra, Indonesia, triggers a tsunami that kills over 230,000 people in 14 countries." },
    '12-27': { year: 1932, event: "Radio City Music Hall Opens", desc: "Radio City Music Hall opens in New York City." },
    '12-28': { year: 1895, event: "First Commercial Film Screening", desc: "The Lumière brothers hold the first public, paid screening of a motion picture at the Grand Café in Paris." },
    '12-29': { year: 1890, event: "Wounded Knee Massacre", desc: "U.S. soldiers kill more than 250 Lakota men, women, and children at Wounded Knee Creek in South Dakota." },
    '12-30': { year: 1922, event: "USSR Established", desc: "The Union of Soviet Socialist Republics (USSR) is officially established." },
    '12-31': { year: 1999, event: "Yeltsin Resigns", desc: "Boris Yeltsin resigns as President of Russia, leaving Vladimir Putin as acting president." },
};

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
            // Deck finished
            setCurrent(null); 
        } else {
            const newDeck = [...deck];
            const next = newDeck.pop();
            setDeck(newDeck);
            setCurrent(next);
        }
    };

    // Initialize
    useEffect(() => { if (!current && deck.length > 0) draw(); }, []);

    return { current, draw, remaining: deck.length, deck };
}

// --- COMPONENTS ---

const Card = ({ children, className = "", onClick, isComplete }) => (
    <div onClick={onClick} className={`bg-gray-800 border ${isComplete ? 'border-green-500 shadow-[0_0_15px_rgba(34,197,94,0.3)]' : 'border-gray-700'} rounded-xl p-6 shadow-lg hover:shadow-xl transition-all duration-300 ${className}`}>
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

const CircularProgress = ({ value, max, size = 36, strokeWidth = 3, colorClass = "text-green-400" }) => {
    const radius = (size - strokeWidth) / 2;
    const circumference = 2 * Math.PI * radius;
    const offset = circumference - (value / max) * circumference;

    return (
        <div className="relative flex items-center justify-center" style={{ width: size, height: size }}>
            <svg className="absolute" width={size} height={size} viewBox={`0 0 ${size} ${size}`}>
                <circle
                    className="text-gray-700"
                    stroke="currentColor"
                    strokeWidth={strokeWidth}
                    fill="transparent"
                    r={radius}
                    cx={size / 2}
                    cy={size / 2}
                />
                <circle
                    className={`${colorClass} transition-all duration-300`}
                    stroke="currentColor" strokeWidth={strokeWidth} fill="transparent" r={radius} cx={size / 2} cy={size / 2}
                    strokeDasharray={circumference} strokeDashoffset={offset} strokeLinecap="round"
                    style={{ transform: 'rotate(-90deg)', transformOrigin: '50% 50%' }} />
            </svg>
            <span className="text-xs font-bold text-gray-400">{max - value}</span>
        </div>
    );
};

// --- GAMES ---

const PiGame = ({ onAddPoints, onComplete }) => {
    const [input, setInput] = useState("");
    const [error, setError] = useState(false);
    const [revealed, setRevealed] = useState(false);

    const handleChange = (e) => {
        const val = e.target.value;
        if (val.length > input.length) {
             // Added a char
             if (val[val.length - 1] !== PI_100[val.length - 1]) {
                setError(true);
                setTimeout(() => setError(false), 500);
            } else {
                // Correct
                setInput(val);
                setError(false);
                onAddPoints(1); // 1 point per digit
                if (val.length === PI_100.length) {
                    onComplete();
                }
            }
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

const PeriodicGame = ({ onAddPoints, onComplete }) => {
    const total = ELEMENTS.length;
    const { current, draw, remaining } = useDeck(ELEMENTS);
    const [options, setOptions] = useState([]);
    const [score, setScore] = useState(0);
    const [feedback, setFeedback] = useState(null);

    useEffect(() => {
        if (!current) return;
        const distractors = ELEMENTS.filter(e => e.n !== current.n).sort(() => 0.5 - Math.random()).slice(0, 3);
        setOptions([current, ...distractors].sort(() => 0.5 - Math.random()));
        setFeedback(null);
    }, [current]);

    const handleAnswer = (selected) => {
        if (feedback) return;
        if (selected.n === current.n) {
            setScore(s => s + 1);
            onAddPoints(10);
            setFeedback('correct');
            setTimeout(() => {
                if (remaining === 0) onComplete();
                draw();
            }, 800);
        } else {
            setScore(0);
            setFeedback('wrong');
            setTimeout(draw, 1500);
        }
    };

    if (!current && remaining === 0) return <div className="text-center text-2xl text-green-400 font-bold p-10">Periodic Table Mastered!</div>;
    if (!current) return null;

    return (
        <div className="max-w-xl mx-auto animate-fade-in">
            <div className="flex justify-between items-center mb-8">
                <CircularProgress value={total - remaining} max={total} colorClass="text-green-400" />
                <div className="bg-gray-800 px-4 py-2 rounded-full border border-gray-700 text-green-400 font-bold">Streak: {score}</div>
            </div>
            <div className="flex justify-center mb-8">
                <div className="w-40 h-40 bg-gray-100 text-gray-900 rounded-lg flex flex-col items-center justify-center shadow-2xl border-4 border-white relative">
                    <span className="absolute top-2 left-3 text-lg font-bold text-gray-500">{current.n}</span>
                    <span className="text-6xl font-bold">{current.s}</span>
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

const FlagGame = ({ onAddPoints, onComplete }) => {
    const total = FLAGS.length;
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
            onAddPoints(10);
            setFeedback('correct');
            setTimeout(() => {
                if (remaining === 0) onComplete();
                draw();
            }, 800);
        } else {
            setScore(0);
            setFeedback('wrong');
            setTimeout(draw, 1500);
        }
    };

    if (!current && remaining === 0) return <div className="text-center text-2xl text-green-400 font-bold p-10">All Flags Identified!</div>;
    if (!current) return <div className="text-center">Loading Deck...</div>;

    return (
        <div className="max-w-2xl mx-auto animate-fade-in">
            <div className="flex justify-between items-center mb-4">
                <div className="text-gray-500 text-sm uppercase tracking-widest font-semibold">{score} Correct in a row</div>
                <CircularProgress value={total - remaining} max={total} colorClass="text-yellow-400" />
            </div>
            <div className="flex flex-col items-center justify-center mb-8 h-48 md:h-64">
                <img src={`https://flagcdn.com/w320/${current.code}.png`} alt="Flag" className="h-full rounded-lg shadow-2xl object-contain" />
            </div>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                {options.map((opt, i) => (
                    <button key={i} onClick={() => handleAnswer(opt)} className={`p-4 rounded-xl text-lg font-medium transition-all border-2 ${feedback === 'correct' && opt.n === current.n ? 'bg-green-500/20 border-green-500 text-green-400' : feedback === 'wrong' && opt.n === current.n ? 'bg-green-500/20 border-green-500 text-green-400' : feedback === 'wrong' && opt.n !== current.n ? 'opacity-30' : 'bg-gray-800 border-gray-700 hover:bg-gray-700'}`}>{opt.n}</button>
                ))}
            </div>
        </div>
    );
};

const GeoGame = ({ onAddPoints, onComplete }) => {
    const total = FLAGS_WITH_CAPITALS.length;
    const { current, draw, remaining } = useDeck(FLAGS_WITH_CAPITALS);
    const [answer, setAnswer] = useState("");
    const [reveal, setReveal] = useState(false);

    const check = () => {
        const target = current.capital;
        if (answer.toLowerCase().trim() === target.toLowerCase()) {
            setReveal(true);
            onAddPoints(10);
            if (remaining === 0) onComplete();
        }
    };

    if (!current && remaining === 0) return <div className="text-center text-2xl text-green-400 font-bold p-10">All Capitals Mastered!</div>;
    if (!current) return null;

    return (
        <div className="max-w-lg mx-auto animate-fade-in">
            <div className="flex justify-end mb-4">
                <CircularProgress value={total - remaining} max={total} colorClass="text-blue-400" />
            </div>
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
                        <Button onClick={() => { setAnswer(""); setReveal(false); draw(); }} variant="primary">Next Country</Button>
                    </div>
                )}
            </Card>
        </div>
    );
};

const LanguageGame = ({ onAddPoints, onComplete }) => {
    const total = GREETINGS.length;
    const { current, draw, remaining } = useDeck(GREETINGS);
    const [isFlipped, setIsFlipped] = useState(false);
    const [isAnimating, setIsAnimating] = useState(false);
    const [input, setInput] = useState("");
    const [feedback, setFeedback] = useState(null);

    const handleNext = () => {
        if (isAnimating) return;
        setIsFlipped(false);
        setIsAnimating(true);
        setTimeout(() => {
            draw();
            setInput("");
            setFeedback(null);
            setIsAnimating(false);
        }, 300);
    };

    const checkAnswer = () => {
        if (input.toLowerCase().trim() === current.w.toLowerCase()) {
            onAddPoints(10);
            setFeedback("correct");
            setIsFlipped(true);
            if (remaining === 0) onComplete();
        } else {
            setFeedback("wrong");
        }
    };

    const handleFlip = () => {
         if (!isFlipped) setIsFlipped(true);
         else handleNext();
    };

    if (!current && remaining === 0) return <div className="text-center text-2xl text-green-400 font-bold p-10">All Languages Learned!</div>;
    if (!current) return <div>Loading...</div>;

    return (
        <div className="max-w-md mx-auto h-96 perspective-1000 animate-fade-in">
            <div className="flex justify-end mb-4">
                <CircularProgress value={total - remaining} max={total} colorClass="text-purple-400" />
            </div>
            <div className="relative w-full h-full cursor-pointer transition-transform duration-500 transform-style-3d" style={{ transform: isFlipped ? 'rotateY(180deg)' : '' }}>
                
                {/* Front Side */}
                <div className="absolute w-full h-full bg-gradient-to-br from-purple-600 to-blue-800 rounded-2xl flex flex-col items-center justify-between p-8 backface-hidden shadow-2xl border border-white/10">
                    <div className="flex-1 flex flex-col items-center justify-center">
                        <h3 className="text-xl text-purple-200 mb-2">How do you say Hello in</h3>
                        <h2 className="text-4xl font-bold text-white">{current.l}?</h2>
                    </div>
                    
                    {/* Input Area (Only on Front) */}
                    <div className="w-full mt-4" onClick={(e) => e.stopPropagation()}>
                        <input 
                            type="text" 
                            value={input} 
                            onChange={(e) => setInput(e.target.value)}
                            className={`w-full p-3 rounded-lg bg-black/30 border text-white placeholder-purple-200/50 outline-none mb-2 ${feedback === 'wrong' ? 'border-red-400 animate-wiggle' : 'border-white/20'}`}
                            placeholder="Type greeting..."
                            onKeyDown={(e) => e.key === 'Enter' && checkAnswer()}
                        />
                        <div className="flex gap-2">
                             <Button onClick={checkAnswer} variant="success" className="flex-1">Check</Button>
                             <Button onClick={handleFlip} variant="secondary" className="flex-1">Give Up</Button>
                        </div>
                    </div>
                </div>

                {/* Back Side */}
                <div className="absolute w-full h-full bg-gray-800 rounded-2xl flex flex-col items-center justify-center p-8 backface-hidden shadow-2xl border border-gray-700" style={{ transform: 'rotateY(180deg)' }} onClick={handleNext}>
                    <h2 className="text-5xl font-bold text-green-400 mb-8">{current.w}</h2>
                    <p className="text-gray-400 text-sm">Click to Next</p>
                </div>
            </div>
        </div>
    );
};

const DishGame = ({ onAddPoints, onComplete }) => {
    const total = DISHES.length;
    const { current, draw, remaining } = useDeck(DISHES);
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
            onAddPoints(10);
            setFeedback("correct");
            setIsFlipped(true);
            if (remaining === 0) onComplete();
        } else {
            setFeedback("wrong");
        }
    };

    if (!current && remaining === 0) return <div className="text-center text-2xl text-green-400 font-bold p-10">All Dishes Tasted!</div>;
    if (!current) return <div>Loading...</div>;

    const frontLabel = showCountryFirst ? "What is the national dish of" : "Which country's national dish is";
    const frontValue = showCountryFirst ? current.c : current.d;
    const backLabel = showCountryFirst ? "National Dish" : "Country";
    const backValue = showCountryFirst ? current.d : current.c;

    return (
        <div className="max-w-md mx-auto animate-fade-in flex flex-col items-center">
            <div className="w-full flex justify-between items-center mb-4 px-4">
                <div className="text-orange-400 font-bold">Score: {score}</div>
                <CircularProgress value={total - remaining} max={total} colorClass="text-orange-400" />
            </div>

            <div className="relative w-80 h-80 md:w-96 md:h-96 cursor-pointer perspective-1000 mb-6">
                <div className="relative w-full h-full transition-transform duration-700 transform-style-3d" style={{ transform: isFlipped ? 'rotateY(180deg)' : '' }}>
                {/* Front Side */}
                    <div className="absolute w-full h-full bg-white rounded-full flex flex-col items-center justify-center p-8 backface-hidden shadow-2xl border-[16px] border-gray-300 text-center">
                        <div className="absolute inset-8 border-2 border-gray-200 rounded-full"></div>
                        <div className="relative z-10 flex flex-col items-center justify-center">
                            <Utensils className="text-gray-500 mb-4" size={32}/>
                            <h3 className="text-lg text-gray-600 font-semibold mb-2">{frontLabel}</h3>
                            
                            {showCountryFirst && (
                                 <img src={`https://flagcdn.com/w160/${current.code}.png`} alt="flag" className="h-12 mb-4 rounded shadow-md object-contain"/>
                            )}

                            <h2 className="text-3xl font-bold text-gray-800">{frontValue}?</h2>
                        </div>
                    </div>

                {/* Back Side */}
                    <div className="absolute w-full h-full bg-white rounded-full flex flex-col items-center justify-center p-8 backface-hidden shadow-2xl border-[16px] border-gray-300" style={{ transform: 'rotateY(180deg)' }}>
                        <div className="absolute inset-8 border-2 border-green-200 rounded-full"></div>
                        <div className="relative z-10 flex flex-col items-center justify-center">
                            <h3 className="text-sm text-gray-500 mb-2">{backLabel}</h3>
                            {!showCountryFirst && (
                                 <img src={`https://flagcdn.com/w160/${current.code}.png`} alt="flag" className="h-16 mb-4 rounded shadow-lg object-contain"/>
                            )}
                            <h2 className="text-3xl font-bold text-green-600 text-center">{backValue}</h2>
                            <Button onClick={(e) => { e.stopPropagation(); next(); }} variant="primary" className="mt-6 bg-orange-600 hover:bg-orange-500 shadow-lg shadow-orange-500/20">Next</Button>
                        </div>
                    </div>
                </div>
            </div>

            <div className="w-full max-w-sm" onClick={(e) => e.stopPropagation()}>
                <input 
                    type="text" 
                    value={input} 
                    onChange={(e) => setInput(e.target.value)}
                    className={`w-full p-3 rounded-lg bg-gray-800 border-2 text-white text-center placeholder-gray-500 outline-none mb-2 ${feedback === 'wrong' ? 'border-red-400 animate-wiggle' : 'border-gray-600 focus:border-orange-500'}`}
                    placeholder="Type guess..."
                    onKeyDown={(e) => e.key === 'Enter' && checkAnswer()}
                />
                <div className="flex gap-2">
                    <Button onClick={checkAnswer} variant="success" className="flex-1">Guess</Button>
                    <Button onClick={() => setIsFlipped(true)} variant="secondary" className="flex-1">Give Up</Button>
                </div>
            </div>
        </div>
    );
};

const PopulationGame = ({ onAddPoints, onComplete }) => {
    const total = POPULATION.length;
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
            onAddPoints(10);
            setFeedback('correct');
            setTimeout(() => {
                if (remaining === 0) onComplete();
                draw();
            }, 1000);
        } else {
            setScore(0);
            setFeedback('wrong');
            setTimeout(draw, 2000);
        }
    };

    if (!current && remaining === 0) return <div className="text-center text-2xl text-green-400 font-bold p-10">All Populations Estimated!</div>;
    if (!current) return <div>Loading...</div>;

    return (
        <div className="max-w-md mx-auto animate-fade-in">
            <div className="flex justify-end mb-4">
                <CircularProgress value={total - remaining} max={total} colorClass="text-indigo-400" />
            </div>
            <Card className="text-center mb-6 bg-slate-100 border-slate-300 text-slate-800">
                <div className="flex justify-between items-center border-b-2 border-slate-300 pb-2 mb-4">
                    <h3 className="font-bold text-lg">Population Report</h3>
                    <Users className="text-slate-600" size={24}/>
                </div>
                <div className="text-slate-600 text-sm mb-2">Estimate the population of:</div>
                <h2 className="text-4xl font-bold text-slate-900 mb-2">{current.c}</h2>
                <div className="text-xs text-green-600 font-bold tracking-widest mt-4">STREAK: {score}</div>
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
const AllianceGame = ({ onAddPoints, onComplete }) => {
    const total = ALLIANCES.length;
    const [view, setView] = useState('MENU'); // MENU, GAME
    const [selectedAlliance, setSelectedAlliance] = useState(null);
    const [input, setInput] = useState("");
    const [guessed, setGuessed] = useState(new Set());
    const [flash, setFlash] = useState(false);
    const [showCheatSheet, setShowCheatSheet] = useState(false);
    const [hasPeeked, setHasPeeked] = useState(false);
    const [completedAlliances, setCompletedAlliances] = useState(() => {
        // Load from local storage if available
        try {
            const saved = localStorage.getItem('completedAlliances');
            return saved ? JSON.parse(saved) : [];
        } catch (e) { return []; }
    });

    useEffect(() => {
        localStorage.setItem('completedAlliances', JSON.stringify(completedAlliances));
        if (completedAlliances.length === ALLIANCES.length) {
            onComplete();
        }
    }, [completedAlliances]);

    const startAlliance = (alliance) => {
        setSelectedAlliance(alliance);
        setGuessed(new Set());
        setInput("");
        setHasPeeked(false);
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
            if (!hasPeeked) onAddPoints(5);
            setInput("");
            
            // Check completion
            if (newGuessed.size === selectedAlliance.members.length) {
                if (!completedAlliances.includes(selectedAlliance.id)) {
                    setCompletedAlliances([...completedAlliances, selectedAlliance.id]);
                    onAddPoints(100); // Bonus for set completion
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
                                className={`p-6 rounded-lg text-left border-2 transition-all hover:scale-105 relative overflow-hidden bg-slate-800/50 backdrop-blur-sm
                                    ${isComplete ? 'border-green-500/50' : 'border-slate-700 hover:border-blue-500'}`}
                            >
                                <div className="flex justify-between items-start">
                                    <div>
                                        <div className="font-sans font-bold text-2xl mb-1 text-slate-100">{a.name}</div>
                                        <div className="text-xs text-slate-400">{a.members.length} Members</div>
                                    </div>
                                    {isComplete ? <Lock size={20} className="text-green-500" /> : <Unlock size={20} className="text-slate-500" />}
                                </div>
                                {isComplete && <div className="absolute bottom-0 left-0 w-full h-1 bg-green-500"></div>}
                            </button>
                        )
                    })}
                </div>
            </div>
        );
    }

    if (showCheatSheet) {
        return (
             <div className="fixed inset-0 bg-gray-950 z-[100] overflow-hidden flex flex-col animate-fade-in">
                <div className="absolute inset-0 overflow-auto p-8">
                     <div className="max-w-4xl mx-auto">
                        <div className="flex justify-between items-center mb-8 sticky top-0 bg-gray-950/90 backdrop-blur p-4 border-b border-gray-800 z-10">
                             <h2 className="text-3xl font-bold text-blue-100">{selectedAlliance.name} Members</h2>
                             <Button onClick={() => setShowCheatSheet(false)} variant="secondary">Close</Button>
                        </div>
                        <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                            {selectedAlliance.members.map((m, i) => (
                                <div key={i} className="flex justify-between items-center p-4 bg-gray-900 rounded-lg border border-gray-800">
                                    <span className="text-lg font-bold text-white">{m}</span>
                                </div>
                            ))}
                        </div>
                     </div>
                </div>
            </div>
        )
    }

    // GAME VIEW
    const totalMembers = selectedAlliance.members.length;
    const remaining = total - guessed.size;

    return (
        <div className="max-w-4xl mx-auto animate-fade-in">
            <div className="flex justify-between items-center mb-4 text-sm text-gray-500">
                 <button onClick={() => setView('MENU')} className="mb-4 flex items-center text-gray-400 hover:text-white"><ChevronLeft size={16}/> Alliances</button>
                 <button onClick={() => { setShowCheatSheet(true); setHasPeeked(true); }} className="flex items-center gap-2 text-blue-400 hover:text-blue-300 font-bold">
                    <Search size={16}/> Cheat Sheet {hasPeeked && "(Peeked)"}
                </button>
            </div>

            <div className="text-center mb-8">
                <h2 className="text-5xl font-black text-blue-500 mb-2 tracking-tight">{selectedAlliance.name}</h2>
                <p className="text-gray-400 text-lg">{selectedAlliance.full}</p>
                <div className="mt-4 text-sm font-bold bg-gray-800 inline-block px-4 py-1 rounded-full text-gray-300">
                    {guessed.size} / {totalMembers} Members
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
const EmpireGame = ({ onAddPoints, onComplete }) => {
    const totalEmpires = EMPIRES.length;
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
        if (completedEmpires.length === EMPIRES.length) {
            onComplete();
        }
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
            onAddPoints(5);
            setInput("");
            
            if (newGuessed.size === selectedEmpire.members.length) {
                if (!completedEmpires.includes(selectedEmpire.id)) {
                    setCompletedEmpires([...completedEmpires, selectedEmpire.id]);
                    onAddPoints(100);
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
                                className={`p-6 rounded-lg text-left border-2 transition-all hover:scale-105 relative overflow-hidden bg-slate-800/50 backdrop-blur-sm
                                    ${isComplete ? 'bg-yellow-900/30 border-yellow-500/50' : 'border-slate-700 hover:border-yellow-500'}`}
                            >
                                <div className="flex justify-between items-start">
                                    <div>
                                        <div className="font-serif font-bold text-2xl mb-1 text-slate-100">{e.name}</div>
                                        <div className="text-xs text-slate-400">{e.members.length} Modern Territories</div>
                                    </div>
                                    {isComplete ? <Lock size={20} className="text-yellow-500" /> : <Unlock size={20} className="text-slate-500" />}
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

// --- CURRENCY GAME ---
const CurrencyGame = ({ onAddPoints, onComplete }) => {
    const [view, setView] = useState('MENU');
    const [selectedCurrency, setSelectedCurrency] = useState(null);
    const [input, setInput] = useState("");
    const [guessed, setGuessed] = useState(new Set());
    const [flash, setFlash] = useState(false);
    const [showCheatSheet, setShowCheatSheet] = useState(false);
    const [hasPeeked, setHasPeeked] = useState(false);
    const [completedCurrencies, setCompletedCurrencies] = useState(() => {
        try {
            const saved = localStorage.getItem('completedCurrencies');
            return saved ? JSON.parse(saved) : [];
        } catch (e) { return []; }
    });

    useEffect(() => {
        localStorage.setItem('completedCurrencies', JSON.stringify(completedCurrencies));
        if (completedCurrencies.length === CURRENCIES.length) {
            onComplete();
        }
    }, [completedCurrencies]);

    const startCurrency = (c) => {
        setSelectedCurrency(c);
        setGuessed(new Set());
        setInput("");
        setHasPeeked(false);
        setView('GAME');
    };

    const checkAnswer = (val) => {
        const cleanVal = normalize(val);
        const match = selectedCurrency.members.find(m => {
            const normM = normalize(m);
            if (normM === cleanVal) return true;
            if (COUNTRY_ALIASES[cleanVal] === m) return true;
            return false;
        });

        if (match && !guessed.has(match)) {
            const newGuessed = new Set(guessed);
            newGuessed.add(match);
            setGuessed(newGuessed);
            if (!hasPeeked) onAddPoints(5);
            setInput("");
            
            if (newGuessed.size === selectedCurrency.members.length) {
                if (!completedCurrencies.includes(selectedCurrency.id)) {
                    setCompletedCurrencies([...completedCurrencies, selectedCurrency.id]);
                    onAddPoints(100);
                }
            }
        } else if (match && guessed.has(match)) {
            setFlash(true);
            setTimeout(() => setFlash(false), 300);
        }
    };

    if (view === 'MENU') {
        return (
            <div className="max-w-4xl mx-auto animate-fade-in text-center">
                <h2 className="text-3xl font-bold mb-8 text-emerald-400">Select a Currency</h2>
                <div className="grid grid-cols-2 md:grid-cols-3 gap-6 place-items-center">
                    {CURRENCIES.map((c, i) => {
                        const isComplete = completedCurrencies.includes(c.id);
                        const coinTypes = ['gold', 'silver', 'bronze'];
                        const type = coinTypes[i % 3];
                        const colors = {
                            gold: 'bg-gradient-to-br from-yellow-400 to-amber-600 text-amber-900 border-amber-700 hover:shadow-yellow-300/50',
                            silver: 'bg-gradient-to-br from-slate-300 to-slate-500 text-slate-800 border-slate-600 hover:shadow-slate-300/50',
                            bronze: 'bg-gradient-to-br from-orange-500 to-amber-700 text-amber-900 border-amber-800 hover:shadow-orange-400/50',
                        };
                        return (
                            <button 
                                key={c.id}
                                onClick={() => startCurrency(c)}
                                className={`w-40 h-40 rounded-full flex flex-col items-center justify-center text-center p-2 border-4 shadow-lg transition-all hover:scale-105 ${colors[type]} ${isComplete ? 'opacity-60' : ''}`}
                            >
                                <span className="font-black text-lg leading-tight">{c.name.split(' (')[0]}</span>
                                <span className="text-4xl font-mono font-bold">{c.name.match(/\((.*)\)/)?.[1]}</span>
                                {isComplete && <Lock size={20} className="mt-1" />}
                            </button>
                        )
                    })}
                </div>
            </div>
        );
    }

    if (showCheatSheet) {
        return (
            <div className="fixed inset-0 bg-gray-950 z-[100] overflow-hidden flex flex-col animate-fade-in">
                <div className="absolute inset-0 overflow-auto p-8">
                     <div className="max-w-4xl mx-auto">
                        <div className="flex justify-between items-center mb-8 sticky top-0 bg-gray-950/90 backdrop-blur p-4 border-b border-gray-800 z-10">
                             <h2 className="text-3xl font-bold text-emerald-100">Currency List</h2>
                             <Button onClick={() => setShowCheatSheet(false)} variant="secondary">Close</Button>
                        </div>
                        <div className="space-y-6">
                            {CURRENCIES.map((c, i) => (
                                <div key={i} className="bg-gray-900 rounded-lg border border-gray-800 p-6">
                                    <h3 className="text-2xl font-bold text-emerald-400 mb-4">{c.name}</h3>
                                    <div className="flex flex-wrap gap-2">
                                        {c.members.map((m, j) => (
                                            <span key={j} className="px-3 py-1 bg-gray-800 rounded-full text-sm text-gray-300">{m}</span>
                                        ))}
                                    </div>
                                </div>
                            ))}
                        </div>
                     </div>
                </div>
            </div>
        )
    }

    const totalMembers = selectedCurrency.members.length;
    const remaining = totalMembers - guessed.size;

    return (
        <div className="max-w-4xl mx-auto animate-fade-in">
            <div className="flex justify-between items-center mb-4 text-sm text-gray-500">
                <button onClick={() => setView('MENU')} className="flex items-center gap-1 hover:text-white transition-colors"><ChevronLeft size={16}/> Currencies</button>
                <button onClick={() => { setShowCheatSheet(true); setHasPeeked(true); }} className="flex items-center gap-2 text-emerald-400 hover:text-emerald-300 font-bold">
                    <Search size={16}/> View Cheat Sheet {hasPeeked && "(Peeked)"}
                </button>
            </div>
            <div className="text-center mb-8">
                <h2 className="text-5xl font-black text-emerald-500 mb-2 tracking-tight">{selectedCurrency.name}</h2>
                <div className="mt-4 text-sm font-bold bg-gray-800 inline-block px-4 py-1 rounded-full text-gray-300">
                    {guessed.size} / {totalMembers} Countries
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
                    className={`w-full bg-gray-800 border-2 border-gray-600 rounded-xl px-6 py-4 text-xl text-center outline-none focus:border-emerald-500 transition-all ${flash ? 'border-red-500' : ''}`}
                    placeholder="Type a country..."
                />
            </div>

            <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-3">
                {selectedCurrency.members.map((m, i) => {
                    const isGuessed = guessed.has(m);
                    return (
                        <div 
                            key={i} 
                            className={`h-16 flex items-center justify-center rounded-lg text-sm font-bold transition-all duration-500 px-2 text-center
                            ${isGuessed ? 'bg-emerald-600 text-white scale-100' : 'bg-gray-800 text-transparent scale-95 border border-gray-700'}`}
                        >
                            {isGuessed ? m : ""}
                        </div>
                    );
                })}
            </div>

            {remaining === 0 && (
                <div className="mt-12 text-center animate-fade-in">
                    <h3 className="text-3xl font-bold text-emerald-400 mb-4">All Countries Found!</h3>
                    <Button onClick={() => setView('MENU')} variant="primary" className="mx-auto text-lg px-8 py-3 bg-emerald-600 hover:bg-emerald-500 border-none text-white">
                        Back to Menu <ArrowRight className="ml-2" />
                    </Button>
                </div>
            )}
        </div>
    );
};

// 9. CODE GAME (Reverted to Menu-Based)
const CodeGame = ({ onAddPoints, onComplete }) => {
    const total = CODE_SNIPPETS.length;
    const [selectedLang, setSelectedLang] = useState(null);
    const [input, setInput] = useState("");
    const [feedback, setFeedback] = useState(null);
    const [revealed, setRevealed] = useState(false);
    const [warning, setWarning] = useState("");
    const [completedLangs, setCompletedLangs] = useState([]);

    const check = () => {
        if (!input.trim()) return;
        
        if (input.trim() === selectedLang.c) {
            setFeedback('correct');
            setWarning("");
            onAddPoints(20);
            if (!completedLangs.includes(selectedLang.l)) {
                const newCompleted = [...completedLangs, selectedLang.l];
                setCompletedLangs(newCompleted);
                if (newCompleted.length === CODE_SNIPPETS.length) onComplete();
            }
        } else if (input.trim().toLowerCase() === selectedLang.c.toLowerCase()) {
            setWarning("Code is case sensitive!");
            setFeedback('wrong');
        } else {
            setFeedback('wrong');
            setWarning("");
        }
        
        setTimeout(() => setFeedback(null), 1000);
    };

    if (!selectedLang) {
        return (
            <div className="max-w-4xl mx-auto animate-fade-in">
                <h2 className="text-3xl font-bold text-center mb-8 text-emerald-400">Select Language</h2>
                <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-4">
                    {CODE_SNIPPETS.map((s, i) => {
                        const isDone = completedLangs.includes(s.l);
                        return (
                            <button 
                                key={i} 
                                onClick={() => { setSelectedLang(s); setInput(""); setRevealed(false); setWarning(""); }}
                                className={`p-4 border rounded-lg transition-all text-left relative ${isDone ? 'bg-emerald-900/30 border-emerald-500' : 'bg-gray-800 border-gray-700 hover:border-emerald-500'}`}
                            >
                                <div className="font-mono font-bold text-emerald-400">{s.l}</div>
                                {isDone && <Check className="absolute top-2 right-2 text-emerald-500" size={16}/>}
                            </button>
                        );
                    })}
                </div>
            </div>
        )
    }

    return (
        <div className="max-w-2xl mx-auto animate-fade-in">
            <div className="flex justify-between items-center mb-4">
                <button onClick={() => setSelectedLang(null)} className="flex items-center text-gray-400 hover:text-white"><ChevronLeft size={16}/> Languages</button>
                <CircularProgress value={completedLangs.length} max={total} colorClass="text-emerald-400" />
            </div>
            
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

// --- KEYBOARD REGION GAME (New) ---
const KeyboardRegionGame = ({ onAddPoints, onComplete }) => {
    const total = KEYBOARD_REGIONS.length;
    const { current, draw, remaining } = useDeck(KEYBOARD_REGIONS);
    const [options, setOptions] = useState([]);
    const [score, setScore] = useState(0);
    const [feedback, setFeedback] = useState(null); // 'correct' | 'wrong'

    useEffect(() => {
        if (!current) return;
        const distractors = KEYBOARD_REGIONS.filter(k => k.region !== current.region).sort(() => 0.5 - Math.random()).slice(0, 3);
        setOptions([current, ...distractors].sort(() => 0.5 - Math.random()));
        setFeedback(null);
    }, [current]);

    const handleAnswer = (opt) => {
        if (feedback) return; // Prevent multiple answers
        if (opt.region === current.region) {
            setScore(s => s + 1);
            onAddPoints(10);
            setFeedback('correct');
        } else {
            setScore(0);
            setFeedback('wrong');
        }
    };
    
    const nextQuestion = () => {
        if (remaining === 0) {
            onComplete();
        }
        draw();
    }

    if (!current && remaining === 0) return <div className="text-center text-2xl text-cyan-400 font-bold p-10">Keyboard Geographer!</div>;
    if (!current) return <div>Loading...</div>;

    return (
        <div className="max-w-2xl mx-auto animate-fade-in">
            <div className="flex justify-end mb-4">
                <CircularProgress value={total - remaining} max={total} colorClass="text-teal-400" />
            </div>
            <Card className="text-center py-8 md:py-12 mb-8 border-cyan-500/30 bg-cyan-900/10">
                 <h3 className="text-cyan-400 text-sm uppercase tracking-widest mb-6">Which language or region uses this keyboard?</h3>
                 
                 <div className="flex flex-col items-center gap-1 md:gap-1.5 p-2">
                    {current.rows.map((row, rowIndex) => (
                        <div key={rowIndex} className="flex justify-center gap-1 md:gap-1.5">
                            {row.split(" ").map((key, keyIndex) => (
                                <div key={keyIndex} className="w-7 h-7 md:w-10 md:h-10 rounded bg-gray-700 border border-gray-600 flex items-center justify-center font-bold text-white shadow-md text-xs md:text-base">
                                    {key}
                                </div>
                            ))}
                        </div>
                    ))}
                 </div>
                 <div className="text-xs text-gray-500 mt-4">Keyboard Layout</div>
                 
                <div className="text-xs text-cyan-500 font-bold tracking-widest mt-8">STREAK: {score}</div>
            </Card>

            {!feedback ? (
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                    {options.map((opt, i) => (
                        <button 
                            key={i} 
                            onClick={() => handleAnswer(opt)}
                            className="p-4 rounded-xl text-lg font-bold transition-all border-2 bg-gray-800 border-gray-700 hover:bg-gray-700 hover:border-cyan-500"
                        >
                            {opt.region}
                        </button>
                    ))}
                </div>
            ) : (
                <div className="animate-fade-in text-center">
                    <div className={`p-6 rounded-xl border mb-6 ${feedback === 'correct' ? 'bg-green-900/30 border-green-500/50' : 'bg-red-900/30 border-red-500/50'}`}>
                        <h3 className={`text-2xl font-bold mb-2 ${feedback === 'correct' ? 'text-green-400' : 'text-red-400'}`}>
                            {feedback === 'correct' ? 'Correct!' : 'Incorrect!'}
                        </h3>
                        <p className="text-white text-lg mb-2">The answer is <span className="text-cyan-400 font-bold">{current.region}</span> ({current.name})</p>
                        <div className="bg-black/30 p-4 rounded-lg text-gray-300 text-sm italic border border-white/10">
                            "{current.desc}"
                        </div>
                    </div>
                    <Button onClick={nextQuestion} variant="primary" className="mx-auto w-full max-w-xs">Next <ArrowRight size={16}/></Button>
                </div>
            )}
        </div>
    );
};

// --- MUSIC NOTES GAME (New) ---

const StaffRenderer = ({ item }) => {
    const { clef, y, ledger } = item;
    const staffLines = [40, 50, 60, 70, 80];

    const TrebleClef = () => <path d="M12 58C12 58 18 40 30 40C42 40 42 62 30 62C18 62 18 80 30 80C42 80 42 100 30 100L30 10" transform="translate(0, -5) scale(0.8)" fill="none" stroke="white" strokeWidth="3" />;
    const BassClef = () => <>
        <path d="M11.3-11.7c-4.9,0-8.8,3.9-8.8,8.8,0,4.4,3.2,8,7.4,8.7" transform="translate(15, 65) scale(1.5)" fill="none" stroke="white" strokeWidth="2" />
        <circle cx="32" cy="50" r="2" fill="white" />
        <circle cx="32" cy="70" r="2" fill="white" />
    </>;

    return (
        <svg viewBox="0 0 200 120" className="w-full h-auto">
            {/* Staff Lines */}
            {staffLines.map(ly => <line key={ly} x1="10" y1={ly} x2="190" y2={ly} stroke="rgba(255,255,255,0.5)" strokeWidth="1" />)}
            
            {/* Clef */}
            {clef === 'treble' ? <TrebleClef /> : <BassClef />}

            {/* Note */}
            <ellipse cx="100" cy={y} rx="6" ry="4" fill="white" />
            {/* Stem */}
            {y > 60 ? <line x1="106" y1={y} x2="106" y2={y - 30} stroke="white" strokeWidth="1.5" /> : <line x1="94" y1={y} x2="94" y2={y + 30} stroke="white" strokeWidth="1.5" />}
            
            {/* Ledger Line */}
            {ledger && <line x1="90" y1={y} x2="110" y2={y} stroke="white" strokeWidth="1.5" />}
        </svg>
    );
};

const MusicNotesGame = ({ onAddPoints, onComplete }) => {
    const total = MUSIC_NOTES_DATA.length;
    const { current, draw, remaining } = useDeck(MUSIC_NOTES_DATA);
    const [options, setOptions] = useState([]);
    const [score, setScore] = useState(0);
    const [feedback, setFeedback] = useState(null);
    const [showCheatSheet, setShowCheatSheet] = useState(false);

    useEffect(() => {
        if (!current) return;
        const distractors = MUSIC_NOTES_DATA.filter(n => n.name !== current.name).sort(() => 0.5 - Math.random()).slice(0, 3);
        setOptions([current, ...distractors].sort(() => 0.5 - Math.random()));
        setFeedback(null);
    }, [current]);

    const handleAnswer = (opt) => {
        if (feedback) return;
        if (opt.name === current.name) {
            setScore(s => s + 1);
            onAddPoints(10);
            setFeedback('correct');
        } else {
            setScore(0);
            setFeedback('wrong');
        }
    };

    const nextQuestion = () => {
        if (remaining === 0) onComplete();
        draw();
    };

    if (!current && remaining === 0) return <div className="text-center text-2xl text-purple-400 font-bold p-10">Maestro!</div>;
    if (!current) return <div>Loading...</div>;

    if (showCheatSheet) {
        return (
            <div className="fixed inset-0 bg-gray-950 z-[100] overflow-hidden flex flex-col animate-fade-in">
                <div className="absolute inset-0 overflow-auto p-8">
                     <div className="max-w-4xl mx-auto">
                        <div className="flex justify-between items-center mb-8 sticky top-0 bg-gray-950/90 backdrop-blur p-4 border-b border-gray-800 z-10">
                             <h2 className="text-3xl font-bold text-purple-100">Music Theory Cheat Sheet</h2>
                             <Button onClick={() => setShowCheatSheet(false)} variant="secondary">Close</Button>
                        </div>
                        <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
                            <div className="bg-gray-900 p-6 rounded-lg border border-gray-800">
                                <h3 className="text-xl font-bold text-purple-400 mb-4">Treble Clef Notes</h3>
                                <p className="mb-2 text-gray-300"><strong className="text-white">Lines:</strong> Every Good Boy Deserves Fudge (E, G, B, D, F)</p>
                                <p className="text-gray-300"><strong className="text-white">Spaces:</strong> FACE (F, A, C, E)</p>
                            </div>
                            <div className="bg-gray-900 p-6 rounded-lg border border-gray-800">
                                <h3 className="text-xl font-bold text-purple-400 mb-4">Bass Clef Notes</h3>
                                <p className="mb-2 text-gray-300"><strong className="text-white">Lines:</strong> Good Boys Do Fine Always (G, B, D, F, A)</p>
                                <p className="text-gray-300"><strong className="text-white">Spaces:</strong> All Cows Eat Grass (A, C, E, G)</p>
                            </div>
                            <div className="bg-gray-900 p-6 rounded-lg border border-gray-800 md:col-span-2">
                                <h3 className="text-xl font-bold text-purple-400 mb-4">Key Signatures (Order of Sharps & Flats)</h3>
                                <p className="mb-2 text-gray-300"><strong className="text-white">Sharps (#):</strong> Father Charles Goes Down And Ends Battle</p>
                                <p className="text-gray-300"><strong className="text-white">Flats (b):</strong> Battle Ends And Down Goes Charles' Father</p>
                            </div>
                        </div>
                     </div>
                </div>
            </div>
        )
    }

    return (
        <div className="max-w-2xl mx-auto animate-fade-in">
            <div className="flex justify-between items-center mb-4 text-sm text-gray-500">
                <CircularProgress value={total - remaining} max={total} colorClass="text-purple-400" />
                <button onClick={() => setShowCheatSheet(true)} className="flex items-center gap-2 text-purple-400 hover:text-purple-300 font-bold">
                    <ScrollText size={16}/> Cheat Sheet
                </button>
            </div>
            <Card className="text-center py-8 mb-8 border-purple-500/30 bg-purple-900/10">
                <h3 className="text-purple-400 text-sm uppercase tracking-widest mb-4">What note is this?</h3>
                <div className="max-w-md mx-auto">
                    <StaffRenderer item={current} />
                </div>
                <div className="text-xs text-purple-500 font-bold tracking-widest mt-8">STREAK: {score}</div>
            </Card>

            {!feedback ? (
                <div className="grid grid-cols-2 gap-4">
                    {options.map((opt, i) => (
                        <button key={i} onClick={() => handleAnswer(opt)} className="p-4 rounded-xl text-2xl font-bold transition-all border-2 bg-gray-800 border-gray-700 hover:bg-gray-700 hover:border-purple-500">
                            {opt.label}
                        </button>
                    ))}
                </div>
            ) : (
                <div className="animate-fade-in text-center">
                    <div className={`p-6 rounded-xl border mb-6 ${feedback === 'correct' ? 'bg-green-900/30 border-green-500/50' : 'bg-red-900/30 border-red-500/50'}`}>
                        <h3 className={`text-2xl font-bold mb-2 ${feedback === 'correct' ? 'text-green-400' : 'text-red-400'}`}>
                            {feedback === 'correct' ? 'Correct!' : 'Incorrect!'}
                        </h3>
                        <p className="text-white text-lg mb-2">The note was <span className="text-purple-400 font-bold">{current.name}</span></p>
                    </div>
                    <Button onClick={nextQuestion} variant="primary" className="mx-auto w-full max-w-xs">Next <ArrowRight size={16}/></Button>
                </div>
            )}
        </div>
    );
};

// --- ON THIS DAY GAME (New) ---
const OnThisDayGame = ({ onAddPoints, onComplete }) => {
    const [selectedDate, setSelectedDate] = useState(null);
    const [viewedCount, setViewedCount] = useState(0);

    const handleDateSelect = (month, day) => {
        const event = HISTORICAL_EVENTS_BY_DATE[`${month}-${day}`];
        if (event) {
            setSelectedDate({ month, day, ...event });
            onAddPoints(1);
            setViewedCount(c => c + 1);
            if (viewedCount + 1 >= 30) { // Complete after viewing 30 events
                onComplete();
            }
        } else {
            // In a full implementation, every day would have an event.
            // This is a fallback for the sparse dataset.
            alert("No event found for this date in the current dataset.");
        }
    };

    if (selectedDate) {
        const monthName = new Date(2024, selectedDate.month - 1, 1).toLocaleString('default', { month: 'long' });
        return (
            <div className="max-w-2xl mx-auto animate-fade-in">
                <Button onClick={() => setSelectedDate(null)} variant="secondary" className="mb-4"><ChevronLeft size={16}/> Back to Calendar</Button>
                <Card className="text-center bg-gradient-to-br from-amber-900/50 to-gray-900 border-amber-800/50">
                    <p className="text-amber-300 font-semibold">{monthName} {selectedDate.day}, {selectedDate.year}</p>
                    <h2 className="text-3xl font-bold text-white my-4">{selectedDate.event}</h2>
                    <p className="text-gray-300 leading-relaxed">{selectedDate.desc}</p>
                </Card>
            </div>
        );
    }

    const monthNames = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
    const currentYear = new Date().getFullYear();

    return (
        <div className="max-w-7xl mx-auto animate-fade-in">
            <h2 className="text-3xl font-bold text-center mb-2 text-amber-400">On This Day in History</h2>
            <p className="text-center text-gray-400 mb-8">Select a date to see what happened.</p>
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-6">
                {monthNames.map((month, monthIndex) => {
                    const daysInMonth = new Date(currentYear, monthIndex + 1, 0).getDate();
                    const firstDayOfMonth = new Date(currentYear, monthIndex, 1).getDay(); // 0=Sun, 1=Mon

                    return (
                        <div key={month} className="bg-gray-800/50 rounded-lg p-4 border border-gray-700">
                            <h3 className="font-bold text-xl text-center text-amber-200 mb-4">{month}</h3>
                            <div className="grid grid-cols-7 gap-1 text-xs text-center text-gray-500 mb-2">
                                {['S', 'M', 'T', 'W', 'T', 'F', 'S'].map((d, i) => <div key={i}>{d}</div>)}
                            </div>
                            <div className="grid grid-cols-7 gap-1">
                                {Array.from({ length: firstDayOfMonth }).map((_, i) => <div key={`pad-${i}`}></div>)}
                                {Array.from({ length: daysInMonth }, (_, i) => i + 1).map(day => {
                                    const hasEvent = !!HISTORICAL_EVENTS_BY_DATE[`${monthIndex + 1}-${day}`];
                                    return (
                                        <button
                                            key={day}
                                            onClick={() => handleDateSelect(monthIndex + 1, day)}
                                            disabled={!hasEvent}
                                            className={`aspect-square rounded-full flex items-center justify-center text-sm font-semibold transition-colors
                                                ${hasEvent 
                                                    ? 'bg-gray-700 hover:bg-amber-500 hover:text-black text-white cursor-pointer' 
                                                    : 'text-gray-600 cursor-not-allowed'
                                                }`}
                                        >
                                            {day}
                                        </button>
                                    );
                                })}
                            </div>
                        </div>
                    );
                })}
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

const ConstellationGame = ({ onAddPoints, onComplete }) => {
    const { current, draw, remaining } = useDeck(CONSTELLATIONS);
    const [reveal, setReveal] = useState(false);
    const [viewMode, setViewMode] = useState('GUESS'); // GUESS, MAP

    const handleReveal = () => {
        setReveal(true);
        // No points for revealing, but allows moving forward
    };

    const handleNext = () => {
        setReveal(false);
        onAddPoints(5); // Small point reward for studying
        if (remaining === 0) onComplete();
        draw();
    };

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

    if (!current && remaining === 0) return <div className="text-center text-2xl text-yellow-400 font-bold p-10">Star Master!</div>;
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
                        <Button onClick={handleReveal} variant="primary" className="mx-auto w-full max-w-xs">Reveal Answer</Button>
                    </div>
                ) : (
                    <div className="z-10 text-center animate-fade-in w-full">
                        <h2 className="text-3xl font-bold text-yellow-400 mb-2">{current.name}</h2>
                        <p className="text-gray-300 mb-6 max-w-sm mx-auto">{current.desc}</p>
                        <Button onClick={handleNext} variant="outline" className="mx-auto">Next Constellation</Button>
                    </div>
                )}
            </Card>
        </div>
    );
};

// --- NEW TLD GAME ---
const TLDGame = ({ onAddPoints, onComplete }) => {
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
            onAddPoints(10);
            setFeedback('correct');
            setTimeout(() => {
                if (remaining === 0) onComplete();
                draw();
            }, 1000);
        } else {
            setScore(0);
            setFeedback('wrong');
        }
    };

    if (!current && remaining === 0) return <div className="text-center text-2xl text-blue-400 font-bold p-10">Domain Master!</div>;
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
                    <Button onClick={() => { setFeedback(null); if(remaining === 0) onComplete(); else draw(); }} variant="primary" className="mx-auto w-full max-w-xs">Next Domain <ArrowRight size={16}/></Button>
                </div>
            ) : (
                <div className="grid grid-cols-2 gap-4">
                    {options.map((opt, i) => (
                        <button 
                            key={i} 
                            onClick={() => handleAnswer(opt)}
                            className="p-4 rounded-lg text-xl font-mono font-bold transition-all border-2 bg-gray-800 border-gray-700 hover:bg-gray-700 hover:border-blue-500 flex items-center justify-center gap-2"
                        >
                            <span className="text-gray-500">www.example</span><span className="text-blue-400">{opt.tld}</span>
                        </button>
                    ))}
                </div>
            )}
        </div>
    );
};

// --- PRESIDENTS GAME ---
const PresidentsGame = ({ onAddPoints, onComplete }) => {
    const { current, draw, remaining } = useDeck(US_PRESIDENTS);
    const [options, setOptions] = useState([]);
    const [score, setScore] = useState(0);
    const [feedback, setFeedback] = useState(null);
    const [showCheatSheet, setShowCheatSheet] = useState(false);
    const [hasPeeked, setHasPeeked] = useState(false);

    useEffect(() => {
        if (!current) return;
        const distractors = US_PRESIDENTS.filter(p => p.n !== current.n).sort(() => 0.5 - Math.random()).slice(0, 3);
        setOptions([current, ...distractors].sort(() => 0.5 - Math.random()));
        setFeedback(null);
        setHasPeeked(false);
    }, [current]);

    const handleAnswer = (opt) => {
        if (feedback) return;
        if (opt.n === current.n) {
            setScore(s => s + 1);
            if (!hasPeeked) onAddPoints(10);
            setFeedback('correct');
            setTimeout(() => {
                if (remaining === 0) onComplete();
                draw();
            }, 1000);
        } else {
            setScore(0);
            setFeedback('wrong');
        }
    };

    if (!current && remaining === 0) return <div className="text-center text-2xl text-red-400 font-bold p-10">Hail to the Chief! Completed.</div>;
    if (!current) return <div>Loading...</div>;

    // Helper to add ordinal suffix
    const getOrdinal = (n) => {
        const s = ["th", "st", "nd", "rd"];
        const v = n % 100;
        return n + (s[(v - 20) % 10] || s[v] || s[0]);
    };

    if (showCheatSheet) {
        return (
            <div className="fixed inset-0 bg-gray-950 z-[100] overflow-hidden flex flex-col animate-fade-in">
                <div className="absolute inset-0 overflow-auto p-8">
                     <div className="max-w-4xl mx-auto">
                        <div className="flex justify-between items-center mb-8 sticky top-0 bg-gray-950/90 backdrop-blur p-4 border-b border-gray-800 z-10">
                             <h2 className="text-3xl font-bold text-red-100">US Presidents List</h2>
                             <Button onClick={() => setShowCheatSheet(false)} variant="secondary">Close</Button>
                        </div>
                        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                            {US_PRESIDENTS.map((p, i) => (
                                <div key={i} className="flex items-center p-3 bg-gray-900 rounded border border-gray-800">
                                    <span className="w-8 h-8 bg-red-900/50 rounded-full flex items-center justify-center text-red-200 font-bold mr-3 text-sm">{p.n}</span>
                                    <span className="text-white font-bold">{p.name}</span>
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
                <span>Presidents remaining: {remaining}</span>
                <button onClick={() => { setShowCheatSheet(true); setHasPeeked(true); }} className="flex items-center gap-2 text-red-400 hover:text-red-300 font-bold">
                    <Search size={16}/> Cheat Sheet {hasPeeked && "(Peeked)"}
                </button>
            </div>
            <Card className="text-center mb-8 bg-gradient-to-br from-blue-900 via-slate-900 to-red-900 p-2 border-8 border-double border-amber-200 shadow-2xl">
                <div className="border-4 border-amber-600/50 p-8 rounded-sm">
                    <Crown className="mx-auto text-yellow-400 mb-4" size={40}/>
                    <h3 className="text-gray-400 text-sm uppercase tracking-widest mb-2">Who was the</h3>
                    <h2 className="text-4xl font-bold text-white mb-2">{getOrdinal(current.n)} President?</h2>
                    <div className="text-xs text-blue-400 font-bold tracking-widest mt-4">STREAK: {score}</div>
                </div>
            </Card>

            {feedback ? (
                <div className="animate-fade-in text-center">
                    <div className={`p-6 rounded-xl border mb-6 ${feedback === 'correct' ? 'bg-green-900/30 border-green-500/50' : 'bg-red-900/30 border-red-500/50'}`}>
                        <h3 className={`text-2xl font-bold mb-2 ${feedback === 'correct' ? 'text-green-400' : 'text-red-400'}`}>
                            {feedback === 'correct' ? 'Correct!' : 'Incorrect!'}
                        </h3>
                        <p className="text-white text-lg mb-2">It was <span className="text-yellow-400 font-bold">{current.name}</span></p>
                    </div>
                    <Button onClick={() => { setFeedback(null); if(remaining === 0) onComplete(); else draw(); }} variant="primary" className="mx-auto w-full max-w-xs">Next President <ArrowRight size={16}/></Button>
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
const MonarchsGame = ({ onAddPoints, onComplete }) => {
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
            onAddPoints(10);
            setFeedback('correct');
            setTimeout(() => {
                if (remaining === 0) onComplete();
                draw();
            }, 1000);
        } else {
            setScore(0);
            setFeedback('wrong');
        }
    };

    if (!current && remaining === 0) return <div className="text-center text-2xl text-purple-400 font-bold p-10">Royal Lineage Mastered!</div>;
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
            
            <Card className="text-center mb-8 bg-gradient-to-br from-purple-900/50 to-gray-900 p-2 border-8 border-double border-amber-200 shadow-2xl">
                <div className="border-4 border-purple-600/50 p-8 rounded-sm">
                    <Crown className="mx-auto text-yellow-400 mb-4" size={40}/>
                    <h3 className="text-gray-400 text-sm uppercase tracking-widest mb-2">Who succeeded</h3>
                    <h2 className="text-4xl font-bold font-serif text-white mb-2">{current}?</h2>
                    <div className="text-xs text-purple-400 font-bold tracking-widest mt-4">STREAK: {score}</div>
                </div>
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
const NatoGame = ({ onAddPoints, onComplete }) => {
    const { current, draw, remaining } = useDeck(NATO_PHONETIC);
    const [input, setInput] = useState("");
    const [feedback, setFeedback] = useState(null); // 'correct', 'wrong'
    const [score, setScore] = useState(0);
    const [showCheatSheet, setShowCheatSheet] = useState(false);
    const [hasPeeked, setHasPeeked] = useState(false);

    useEffect(() => {
        if (current) setHasPeeked(false);
    }, [current]);

    const checkAnswer = () => {
        if (feedback) return;
        const fullWord = current.w.toLowerCase();
        const guess = (current.l + input).toLowerCase();
        
        const cleanTarget = fullWord.replace(/[^a-z]/g, '');
        const cleanGuess = guess.replace(/[^a-z]/g, '');

        if (cleanGuess === cleanTarget) {
            setScore(s => s + 1);
            if (!hasPeeked) onAddPoints(10);
            setFeedback('correct');
            setTimeout(() => {
                if (remaining === 0) onComplete();
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

    if (!current && remaining === 0) return <div className="text-center text-2xl text-slate-400 font-bold p-10">NATO Alphabet Mastered!</div>;
    if (!current) return <div>Loading...</div>;

    if (showCheatSheet) {
        return (
            <div className="fixed inset-0 bg-gray-950 z-[100] overflow-hidden flex flex-col animate-fade-in">
                <div className="absolute inset-0 overflow-auto p-8">
                     <div className="max-w-4xl mx-auto">
                        <div className="flex justify-between items-center mb-8 sticky top-0 bg-gray-950/90 backdrop-blur p-4 border-b border-gray-800 z-10">
                             <h2 className="text-3xl font-bold text-slate-100">NATO Phonetic Alphabet</h2>
                             <Button onClick={() => setShowCheatSheet(false)} variant="secondary">Close</Button>
                        </div>
                        <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                            {NATO_PHONETIC.map((item, i) => (
                                <div key={i} className="flex justify-between items-center p-3 bg-gray-900 rounded border border-gray-800">
                                    <span className="font-bold text-slate-400 text-xl">{item.l}</span>
                                    <span className="text-white font-bold">{item.w}</span>
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
                <span>Letters remaining: {remaining}</span>
                <button onClick={() => { setShowCheatSheet(true); setHasPeeked(true); }} className="flex items-center gap-2 text-slate-400 hover:text-slate-300 font-bold">
                    <Search size={16}/> Cheat Sheet {hasPeeked && "(Peeked)"}
                </button>
            </div>
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
const MorseGame = ({ onAddPoints, onComplete }) => {
    const { current, draw, remaining } = useDeck(MORSE_CODE);
    const [options, setOptions] = useState([]);
    const [score, setScore] = useState(0);
    const [feedback, setFeedback] = useState(null);
    const [showCheatSheet, setShowCheatSheet] = useState(false);
    const [hasPeeked, setHasPeeked] = useState(false);

    useEffect(() => {
        if (!current) return;
        // Distractors logic
        const distractors = MORSE_CODE.filter(i => i.l !== current.l).sort(() => 0.5 - Math.random()).slice(0, 3);
        setOptions([current, ...distractors].sort(() => 0.5 - Math.random()));
        setFeedback(null);
        setHasPeeked(false);
    }, [current]);

    const handleAnswer = (opt) => {
        if (feedback) return;
        if (opt.l === current.l) {
            setScore(s => s + 1);
            if (!hasPeeked) onAddPoints(10);
            setFeedback('correct');
            setTimeout(() => {
                if (remaining === 0) onComplete();
                draw();
            }, 800);
        } else {
            setScore(0);
            setFeedback('wrong');
            setTimeout(draw, 1500);
        }
    };

    if (!current && remaining === 0) return <div className="text-center text-2xl text-blue-400 font-bold p-10">Morse Code Mastered!</div>;
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
                <button onClick={() => { setShowCheatSheet(true); setHasPeeked(true); }} className="flex items-center gap-2 text-blue-400 hover:text-blue-300 font-bold">
                    <Search size={16}/> View Alphabet {hasPeeked && "(Peeked)"}
                </button>
            </div>
            
            <Card className="text-center py-8 mb-8 bg-yellow-50 border-yellow-200 shadow-inner">
                <h2 className="text-6xl font-mono font-bold text-gray-800 tracking-widest mb-2">{current.c}</h2>
                <div className="text-xs text-amber-700 font-bold tracking-widest mt-8 uppercase">Decode this symbol</div>
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
const BrailleGame = ({ onAddPoints, onComplete }) => {
    const { current, draw, remaining } = useDeck(BRAILLE_CODE);
    const [options, setOptions] = useState([]);
    const [score, setScore] = useState(0);
    const [feedback, setFeedback] = useState(null);
    const [showCheatSheet, setShowCheatSheet] = useState(false);
    const [hasPeeked, setHasPeeked] = useState(false);

    useEffect(() => {
        if (!current) return;
        const distractors = BRAILLE_CODE.filter(i => i.l !== current.l).sort(() => 0.5 - Math.random()).slice(0, 3);
        setOptions([current, ...distractors].sort(() => 0.5 - Math.random()));
        setFeedback(null);
        setHasPeeked(false);
    }, [current]);

    const handleAnswer = (opt) => {
        if (feedback) return;
        if (opt.l === current.l) {
            setScore(s => s + 1);
            if (!hasPeeked) onAddPoints(10);
            setFeedback('correct');
            setTimeout(() => {
                if (remaining === 0) onComplete();
                draw();
            }, 800);
        } else {
            setScore(0);
            setFeedback('wrong');
            setTimeout(draw, 1500);
        }
    };

    if (!current && remaining === 0) return <div className="text-center text-2xl text-pink-400 font-bold p-10">Braille Mastered!</div>;
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
                <button onClick={() => { setShowCheatSheet(true); setHasPeeked(true); }} className="flex items-center gap-2 text-pink-400 hover:text-pink-300 font-bold">
                    <Glasses size={16}/> View Braille {hasPeeked && "(Peeked)"}
                </button>
            </div>
            
            <Card className="text-center py-16 mb-8 border-pink-500/30 bg-gray-100">
                <h2 className="text-8xl font-bold text-gray-300 mb-2" style={{ textShadow: '1px 1px 1px rgba(0,0,0,0.2), -1px -1px 1px rgba(255,255,255,0.2)' }}>{current.c}</h2>
                <div className="text-xs text-pink-600 font-bold tracking-widest mt-8 uppercase">Decode this symbol</div>
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

// --- BINARY GAME (New) ---
const BinaryGame = ({ onAddPoints, onComplete }) => {
    const { current, draw, remaining } = useDeck(ASCII_DATA);
    const [bits, setBits] = useState(Array(8).fill(0));
    const [score, setScore] = useState(0);
    const [feedback, setFeedback] = useState(null);
    const [showCheatSheet, setShowCheatSheet] = useState(false);
    const [hasPeeked, setHasPeeked] = useState(false);

    useEffect(() => {
        setBits(Array(8).fill(0)); // Reset bits on new card
        setHasPeeked(false);
    }, [current]);

    const toggleBit = (index) => {
        if (feedback) return;
        const newBits = [...bits];
        newBits[index] = newBits[index] === 0 ? 1 : 0;
        setBits(newBits);
    };

    const checkAnswer = () => {
        const userBinary = bits.join("");
        if (userBinary === current.bin) {
            setScore(s => s + 1);
            if (!hasPeeked) onAddPoints(10);
            setFeedback('correct');
            setTimeout(() => {
                if (remaining === 0) onComplete();
                draw();
                setFeedback(null);
            }, 1000);
        } else {
            setScore(0);
            setFeedback('wrong');
            setTimeout(() => {
                setBits(Array(8).fill(0)); // Clear or keep? Let's clear for challenge
                setFeedback(null);
            }, 1000);
        }
    };

    if (!current && remaining === 0) return <div className="text-center text-2xl text-green-400 font-bold p-10">Binary Mastered!</div>;
    if (!current) return <div>Loading...</div>;

    if (showCheatSheet) {
        return (
            <div className="fixed inset-0 bg-gray-950 z-[100] overflow-hidden flex flex-col animate-fade-in">
                <div className="absolute inset-0 overflow-auto p-8">
                     <div className="max-w-4xl mx-auto">
                        <div className="flex justify-between items-center mb-8 sticky top-0 bg-gray-950/90 backdrop-blur p-4 border-b border-gray-800 z-10">
                             <h2 className="text-3xl font-bold text-green-100">ASCII Cheat Sheet</h2>
                             <Button onClick={() => setShowCheatSheet(false)} variant="secondary">Close</Button>
                        </div>
                        <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                            {ASCII_DATA.map((a, i) => (
                                <div key={i} className="flex justify-between items-center p-3 bg-gray-900 rounded border border-gray-800">
                                    <span className="font-bold text-white">{a.char}</span>
                                    <span className="font-mono text-green-400">{a.bin}</span>
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
                <span>Characters remaining: {remaining}</span>
                <button onClick={() => { setShowCheatSheet(true); setHasPeeked(true); }} className="flex items-center gap-2 text-green-400 hover:text-green-300 font-bold">
                    <Search size={16}/> View Binary {hasPeeked && "(Peeked)"}
                </button>
            </div>
            
            <Card className="text-center mb-8 bg-black border-green-500/50 shadow-[0_0_15px_rgba(0,255,0,0.2)]">
                <div className="text-xs text-green-500 font-mono mb-4">TARGET ASCII CHARACTER</div>
                <div className="text-8xl font-mono font-bold text-green-400 mb-4">{current.char}</div>
                <div className="text-xs text-gray-500 font-mono">STREAK: {score}</div>
            </Card>

            <div className="flex justify-center gap-2 mb-8">
                {bits.map((b, i) => (
                    <button 
                        key={i}
                        onClick={() => toggleBit(i)}
                        className={`w-10 h-16 rounded border-2 font-mono text-2xl font-bold transition-all
                            ${b === 1 ? 'bg-green-500 text-black border-green-400 shadow-[0_0_10px_rgba(0,255,0,0.5)]' : 'bg-gray-900 text-gray-600 border-gray-700'}
                        `}
                    >
                        {b}
                    </button>
                ))}
            </div>

            <div className="text-center">
                <Button 
                    onClick={checkAnswer} 
                    variant="success" 
                    className="mx-auto px-8 py-3 text-lg font-mono bg-green-600 hover:bg-green-500 text-black font-bold"
                >
                    EXECUTE
                </Button>
                {feedback === 'wrong' && <div className="text-red-500 font-mono mt-4">INCORRECT BINARY SEQUENCE</div>}
                {feedback === 'correct' && <div className="text-green-400 font-mono mt-4">SEQUENCE ACCEPTED</div>}
            </div>
        </div>
    );
};

// --- CALLING CODES GAME (New) ---
const CallingGame = ({ onAddPoints, onComplete }) => {
    const { current, draw, remaining } = useDeck(CALLING_CODES);
    const [input, setInput] = useState("");
    const [score, setScore] = useState(0);
    const [feedback, setFeedback] = useState(null);
    const [showCheatSheet, setShowCheatSheet] = useState(false);
    const [hasPeeked, setHasPeeked] = useState(false);

    useEffect(() => {
        if (current) setHasPeeked(false);
    }, [current]);

    const handlePress = (num) => {
        if (feedback) return;
        if (input.length < 4) setInput(prev => prev + num);
    };

    const handleDelete = () => {
        if (feedback) return;
        setInput(prev => prev.slice(0, -1));
    };

    const checkAnswer = () => {
        const target = current.code.replace('+', '');
        if (input === target) {
            setScore(s => s + 1);
            if (!hasPeeked) onAddPoints(10);
            setFeedback('correct');
            setTimeout(() => {
                if (remaining === 0) onComplete();
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

    if (!current && remaining === 0) return <div className="text-center text-2xl text-blue-400 font-bold p-10">All Codes Dialed!</div>;
    if (!current) return <div>Loading...</div>;

    if (showCheatSheet) {
        return (
            <div className="fixed inset-0 bg-gray-950 z-[100] overflow-hidden flex flex-col animate-fade-in">
                <div className="absolute inset-0 overflow-auto p-8">
                     <div className="max-w-4xl mx-auto">
                        <div className="flex justify-between items-center mb-8 sticky top-0 bg-gray-950/90 backdrop-blur p-4 border-b border-gray-800 z-10">
                             <h2 className="text-3xl font-bold text-blue-100">International Calling Codes</h2>
                             <Button onClick={() => setShowCheatSheet(false)} variant="secondary">Close</Button>
                        </div>
                        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                            {CALLING_CODES.map((c, i) => (
                                <div key={i} className="flex justify-between items-center p-3 bg-gray-900 rounded border border-gray-800">
                                    <span className="text-white text-sm">{c.country}</span>
                                    <span className="font-mono text-blue-400 font-bold">{c.code}</span>
                                </div>
                            ))}
                        </div>
                     </div>
                </div>
            </div>
        )
    }

    return (
        <div className="max-w-xs mx-auto animate-fade-in">
             <div className="flex justify-between items-center mb-4 text-sm text-gray-500">
                <span>Remaining: {remaining}</span>
                <button onClick={() => { setShowCheatSheet(true); setHasPeeked(true); }} className="flex items-center gap-2 text-blue-400 hover:text-blue-300 font-bold">
                    <Search size={16}/> Codes {hasPeeked && "(Peeked)"}
                </button>
            </div>

            <div className="bg-gray-800 rounded-[3rem] p-6 border-8 border-gray-700 shadow-2xl relative overflow-hidden">
                {/* Notch */}
                <div className="absolute top-0 left-1/2 -translate-x-1/2 w-32 h-6 bg-gray-700 rounded-b-xl z-10"></div>
                
                <div className="mt-8 mb-6 text-center">
                    <div className="text-xs text-gray-400 uppercase tracking-widest mb-2">Calling...</div>
                    <div className="text-xl font-bold text-white leading-tight h-14 flex items-center justify-center px-2">{current.country}</div>
                </div>

                <div className="bg-gray-900 rounded-xl p-4 mb-6 text-center">
                    <div className="text-3xl font-mono text-blue-400 h-10 flex items-center justify-center gap-1">
                        <span className="text-gray-600">+</span>
                        {input}
                        <span className="w-0.5 h-6 bg-blue-400 animate-pulse ml-1"></span>
                    </div>
                </div>

                <div className="grid grid-cols-3 gap-3 mb-6">
                    {[1, 2, 3, 4, 5, 6, 7, 8, 9].map(n => (
                        <button 
                            key={n} 
                            onClick={() => handlePress(n.toString())}
                            className="aspect-square rounded-full bg-gray-700 hover:bg-gray-600 text-2xl font-bold text-white transition-colors flex items-center justify-center"
                        >
                            {n}
                        </button>
                    ))}
                    <button onClick={handleDelete} className="aspect-square rounded-full bg-red-900/50 hover:bg-red-900/70 text-red-200 font-bold flex items-center justify-center">Del</button>
                    <button onClick={() => handlePress("0")} className="aspect-square rounded-full bg-gray-700 hover:bg-gray-600 text-2xl font-bold text-white flex items-center justify-center">0</button>
                    <button onClick={checkAnswer} className="aspect-square rounded-full bg-green-600 hover:bg-green-500 text-white font-bold flex items-center justify-center">
                        <Phone size={24} />
                    </button>
                </div>
                
                {feedback === 'correct' && <div className="absolute inset-0 bg-green-500/20 flex items-center justify-center pointer-events-none"><div className="bg-green-600 text-white px-4 py-2 rounded-full font-bold">Connected</div></div>}
                {feedback === 'wrong' && <div className="absolute inset-0 bg-red-500/20 flex items-center justify-center pointer-events-none"><div className="bg-red-600 text-white px-4 py-2 rounded-full font-bold">Failed</div></div>}
            </div>
            <div className="text-center mt-4 text-gray-500 font-bold">Streak: {score}</div>
        </div>
    );
};

// --- MAIN APP ---

export default function App() {
    const [view, setView] = useState("HOME");
    const [points, setPoints] = useState(0);
    const [completedGames, setCompletedGames] = useState([]);
    
    // List of all game IDs for global completion check
    const ALL_GAME_IDS = [
        "GEO", "FLAGS", "CALLING", "CURRENCY", "DISH", "POP", 
        "PRESIDENTS", "MONARCHS", "ALLIANCE", "EMPIRES", 
        "ELEMENTS", "PI", "STARS", "MUSIC", "ON_THIS_DAY",
        "CODE", "BINARY", "TLD", "KEYBOARD_REGION",
        "LANG", "NATO", "MORSE", "BRAILLE",
    ];

    const GAME_TITLES = {
        "GEO": "Capital Cities",
        "FLAGS": "Vexillology",
        "CALLING": "Calling Codes",
        "CURRENCY": "Currencies",
        "DISH": "National Dishes",
        "POP": "Population Guesser",
        "PRESIDENTS": "US Presidents",
        "MONARCHS": "Royal Lineage",
        "ALLIANCE": "Alliances",
        "EMPIRES": "Empires",
        "ELEMENTS": "The Periodic Table",
        "PI": "100 Digits of Pi",
        "STARS": "Constellations",
        "MUSIC": "Music Notes",
        "ON_THIS_DAY": "On This Day in History",
        "CODE": "Hello World",
        "BINARY": "ASCII Binary",
        "TLD": "Domain Names",
        "KEYBOARD_REGION": "Keyboard Regions",
        "LANG": "Hello Person",
        "NATO": "NATO Phonetic",
        "MORSE": "Morse Code",
        "BRAILLE": "Braille"
    };

    // Load state on mount
    useEffect(() => {
        const savedPoints = localStorage.getItem('useless_trainer_points');
        const savedCompleted = localStorage.getItem('useless_trainer_completed');
        
        if (savedPoints) setPoints(parseInt(savedPoints));
        if (savedCompleted) setCompletedGames(JSON.parse(savedCompleted));
    }, []);

    // Save state on change
    useEffect(() => {
        localStorage.setItem('useless_trainer_points', points.toString());
        localStorage.setItem('useless_trainer_completed', JSON.stringify(completedGames));
    }, [points, completedGames]);

    const addPoints = (amount) => {
        setPoints(prev => prev + amount);
    };

    const completeGame = (gameId) => {
        if (!completedGames.includes(gameId)) {
            const newCompleted = [...completedGames, gameId];
            setCompletedGames(newCompleted);
            addPoints(500); // Bonus for completing a module
            
            // Check global completion
            if (newCompleted.length === ALL_GAME_IDS.length) {
                addPoints(1000); // Global Bonus
                alert("CONGRATULATIONS! You have mastered all modules! +1000 Points");
            }
        }
    };

    const resetProgress = () => {
        // Removed as per request
    };

    const renderGame = () => {
        const commonProps = { onAddPoints: addPoints, onComplete: () => completeGame(view) };
        
        switch (view) {
            case "PI": return <PiGame {...commonProps} />;
            case "ELEMENTS": return <PeriodicGame {...commonProps} />;
            case "FLAGS": return <FlagGame {...commonProps} />;
            case "GEO": return <GeoGame {...commonProps} />;
            case "LANG": return <LanguageGame {...commonProps} />;
            case "ALLIANCE": return <AllianceGame {...commonProps} />;
            case "EMPIRES": return <EmpireGame {...commonProps} />;
            case "DISH": return <DishGame {...commonProps} />;
            case "POP": return <PopulationGame {...commonProps} />;
            case "CODE": return <CodeGame {...commonProps} />;
            case "STARS": return <ConstellationGame {...commonProps} />;
            case "TLD": return <TLDGame {...commonProps} />;
            case "PRESIDENTS": return <PresidentsGame {...commonProps} />;
            case "MONARCHS": return <MonarchsGame {...commonProps} />;
            case "NATO": return <NatoGame {...commonProps} />;
            case "MORSE": return <MorseGame {...commonProps} />;
            case "BRAILLE": return <BrailleGame {...commonProps} />;
            case "BINARY": return <BinaryGame {...commonProps} />;
            case "CALLING": return <CallingGame {...commonProps} />;
            case "CURRENCY": return <CurrencyGame {...commonProps} />;
            case "MUSIC": return <MusicNotesGame {...commonProps} />;
            case "KEYBOARD_REGION": return <KeyboardRegionGame {...commonProps} />;
            case "ON_THIS_DAY": return <OnThisDayGame {...commonProps} />;
            default: return null;
        }
    };

    // --- DYNAMIC HOME PAGE MODULES ---

    const SectionHeader = ({ title, color, completed, total }) => (
        <div className={`col-span-full flex items-center justify-between gap-4 mt-12 mb-4 pb-2 border-b border-${color}-500/30`}>
            <div className="flex items-center gap-3">
                <h3 className={`text-2xl font-bold text-${color}-400`}>{title}</h3>
            </div>
            <div className="flex items-center gap-3">
                <span className="text-sm font-mono text-gray-400">{completed}/{total}</span>
                <CircularProgress value={total - completed} max={total} size={28} strokeWidth={3} colorClass={`text-${color}-400`} />
            </div>
        </div>
    );

    const ModuleBase = ({ children, onClick, isComplete, color, span, className = "" }) => (
        <div 
            onClick={onClick} 
            className={`col-span-1 ${span > 1 ? 'lg:col-span-2' : ''} relative group cursor-pointer bg-gray-800/70 backdrop-blur-sm border ${isComplete ? 'border-green-500/50 shadow-green-500/10' : 'border-gray-700/50'} rounded-2xl shadow-lg hover:shadow-2xl hover:border-${color}-500 transition-all duration-300 overflow-hidden ${className}`}
        >
            {children}
            {isComplete && <div className="absolute top-4 right-4 bg-green-500/20 text-green-400 rounded-full p-1.5 backdrop-blur-sm"><Check size={16} /></div>}
        </div>
    );

    const DefaultModule = ({ title, description, icon, ...props }) => (
        <ModuleBase {...props}>
            <div className="p-6 flex flex-col h-full min-h-[200px]">
                <div className="flex-1">
                    <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5`}>{icon}</div>
                    <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                    <p className="text-sm text-gray-400">{description}</p>
                </div>
                <div className="h-1 w-full bg-gray-700/50 rounded-full mt-4 overflow-hidden">
                    <div className={`h-1 bg-${props.color}-500 group-hover:w-full transition-all duration-500 ${props.isComplete ? 'w-full' : 'w-0'}`}></div>
                </div>
            </div>
        </ModuleBase>
    );

    const PulsingDotsModule = ({ title, description, icon, ...props }) => (
        <ModuleBase {...props}>
            <div className="absolute inset-0 overflow-hidden grid grid-cols-10 grid-rows-10 gap-2 p-2 opacity-30 group-hover:opacity-60 transition-opacity">
                {[...Array(100)].map((_, i) => (
                    <div 
                        key={i}
                        className={`w-full h-full rounded-full animate-pulse-dot bg-${props.color}-500/50`}
                        style={{ animationDelay: `${Math.random() * 4}s` }}
                    />
                ))}
            </div>
            <div className="relative z-10 p-6 flex flex-col h-full min-h-[200px] bg-gradient-to-t from-gray-900/90 to-transparent">
                <div className="flex-1">
                    <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5`}>{icon}</div>
                    <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                    <p className="text-sm text-gray-400">{description}</p>
                </div>
                <div className="h-1 w-full bg-gray-700/50 rounded-full mt-4 overflow-hidden">
                    <div className={`h-1 bg-${props.color}-500 group-hover:w-full transition-all duration-500 ${props.isComplete ? 'w-full' : 'w-0'}`}></div>
                </div>
            </div>
        </ModuleBase>
    );

    const FallingSymbolsModule = ({ title, description, icon, items, ...props }) => (
        <ModuleBase {...props}>
            <div className="absolute inset-0 overflow-hidden">
                {items.map((item, i) => (
                    <span 
                        key={i}
                        className="absolute text-2xl animate-fall"
                        style={{
                            left: `${Math.random() * 95}%`,
                            animationDelay: `${Math.random() * 10}s`,
                            animationDuration: `${5 + Math.random() * 5}s`,
                            opacity: 0.5 + Math.random() * 0.5
                        }}
                    >
                        {item}
                    </span>
                ))}
            </div>
            <div className="relative z-10 p-6 flex flex-col h-full min-h-[200px] bg-gradient-to-t from-gray-900/90 to-transparent">
                 <div className="flex-1">
                    <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5 backdrop-blur-sm`}>{icon}</div>
                    <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                    <p className="text-sm text-gray-400">{description}</p>
                </div>
                <div className="h-1 w-full bg-gray-700/50 rounded-full mt-4 overflow-hidden">
                    <div className={`h-1 bg-${props.color}-500 group-hover:w-full transition-all duration-500 ${props.isComplete ? 'w-full' : 'w-0'}`}></div>
                </div>
            </div>
        </ModuleBase>
    );

    const GlobeModule = ({ title, description, icon, ...props }) => (
        <ModuleBase {...props}>
            <div className="absolute inset-0 flex items-center justify-center overflow-hidden opacity-20 group-hover:opacity-40 transition-opacity text-blue-300">
                <div className="relative w-48 h-48">
                    <div className="absolute inset-0 rounded-full border-2 border-current animate-globe-pulse" style={{ animationDelay: '0s' }}></div>
                    <div className="absolute inset-0 rounded-full border border-current animate-globe-pulse" style={{ animationDelay: '0.5s' }}></div>
                    <div className="absolute inset-0 rounded-full border border-current animate-globe-pulse" style={{ animationDelay: '1s' }}></div>
                    <div className="absolute inset-0 rounded-full border border-current animate-globe-pulse" style={{ animationDelay: '1.5s' }}></div>
                </div>
            </div>
            <div className="relative z-10 p-6 flex flex-col h-full min-h-[200px] bg-gradient-to-t from-gray-900/90 to-transparent">
                <div className="flex-1">
                    <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5`}>{icon}</div>
                    <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                    <p className="text-sm text-gray-400">{description}</p>
                </div>
                <div className="h-1 w-full bg-gray-700/50 rounded-full mt-4 overflow-hidden">
                    <div className={`h-1 bg-${props.color}-500 group-hover:w-full transition-all duration-500 ${props.isComplete ? 'w-full' : 'w-0'}`}></div>
                </div>
            </div>
        </ModuleBase>
    );

    const DialerModule = ({ title, description, icon, ...props }) => (
        <ModuleBase {...props}>
            <div className="absolute inset-0 overflow-hidden p-4 flex items-center justify-center">
                <div className="grid grid-cols-3 gap-3 w-4/5 max-w-[180px] opacity-20 group-hover:opacity-40 transition-opacity">
                    {['1', '2', '3', '4', '5', '6', '7', '8', '9', '*', '0', '#'].map((key, i) => (
                        <div 
                            key={i}
                            className="aspect-square rounded-full bg-gray-700 flex items-center justify-center font-mono text-2xl text-gray-400 animate-dial-press"
                            style={{ animationDelay: `${Math.random() * 8}s` }}
                        >
                            {key}
                        </div>
                    ))}
                </div>
            </div>
            <div className="relative z-10 p-6 flex flex-col h-full min-h-[200px] bg-gradient-to-t from-gray-900/90 to-transparent">
                <div className="flex-1">
                    <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5`}>{icon}</div>
                    <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                    <p className="text-sm text-gray-400">{description}</p>
                </div>
                <div className="h-1 w-full bg-gray-700/50 rounded-full mt-4 overflow-hidden">
                    <div className={`h-1 bg-${props.color}-500 group-hover:w-full transition-all duration-500 ${props.isComplete ? 'w-full' : 'w-0'}`}></div>
                </div>
            </div>
        </ModuleBase>
    );

    const MatrixModule = ({ title, description, icon, data, ...props }) => (
        <ModuleBase {...props}>
            <div className="absolute inset-0 opacity-10 group-hover:opacity-20 transition-opacity duration-500 overflow-hidden font-mono text-xs leading-tight text-green-400">
                <div className="animate-scroll-vertical">
                    <div className="break-all whitespace-pre-wrap pb-2">
                        {data.repeat(50)}
                    </div>
                    <div className="break-all whitespace-pre-wrap pb-2">{data.repeat(50)}</div>
                </div>
            </div>
            <div className="relative z-10 p-6 h-full flex flex-col justify-end bg-gradient-to-t from-gray-900 via-gray-900/80 to-transparent">
                <div>
                    <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5`}>{icon}</div>
                    <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                    <p className="text-sm text-gray-400">{description}</p>
                </div>
            </div>
        </ModuleBase>
    );

    const TypingModule = ({ title, description, icon, code, ...props }) => (
        <ModuleBase {...props}>
            <div className="p-6 flex flex-col justify-between h-full min-h-[200px]">
                <div>
                    <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5`}>{icon}</div>
                    <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                    <p className="text-sm text-gray-400">{description}</p>
                </div>
                <div className="font-mono text-emerald-400 bg-black/50 p-3 rounded-md text-xs mt-4 typing-effect-container">
                    <p>{code}</p>
                </div>
            </div>
        </ModuleBase>
    );

    const StarfieldModule = ({ title, description, icon, ...props }) => {
        const randomConstellation = useMemo(() => CONSTELLATIONS[Math.floor(Math.random() * CONSTELLATIONS.length)], []);
        return (
            <ModuleBase {...props} className="bg-black">
                <div className="absolute inset-0 opacity-50 group-hover:opacity-100 transition-opacity duration-500">
                    {[...Array(props.span > 1 ? 100 : 40)].map((_, i) => (
                        <div key={i} className="absolute bg-white rounded-full animate-twinkle" style={{ top: `${Math.random()*100}%`, left: `${Math.random()*100}%`, width: `${Math.random()*2}px`, height: `${Math.random()*2}px`, animationDelay: `${Math.random()*5}s`, animationDuration: `${2+Math.random()*3}s` }} />
                    ))}
                </div>
                <div className="absolute inset-0 flex items-center justify-center opacity-50 group-hover:opacity-100 transition-opacity duration-500">
                    <ConstellationRenderer stars={randomConstellation.stars} lines={randomConstellation.lines} size={props.span > 1 ? 200 : 120} animated={true} />
                </div>
                <div className="relative z-10 p-6 min-h-[200px]">
                    <div className={`p-3 mb-4 inline-block bg-${props.color}-500/20 rounded-lg text-${props.color}-400 backdrop-blur-sm border border-white/10`}>{icon}</div>
                    <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                    <p className="text-sm text-gray-400">{description}</p>
                </div>
            </ModuleBase>
        );
    };

    const SlideshowModule = ({ title, description, icon, items, ...props }) => {
        const [index, setIndex] = useState(0);
        useEffect(() => {
            if (!items || items.length === 0) return;
            const interval = setInterval(() => {
                setIndex(prev => (prev + 1) % items.length);
            }, 2500);
            return () => clearInterval(interval);
        }, [items]);

        return (
            <ModuleBase {...props}>
                <div className="absolute inset-0 w-full h-full">
                    {items && items.map((item, i) => (
                        <div 
                            key={i}
                            className="absolute inset-0 w-full h-full bg-cover bg-center transition-opacity duration-1000"
                            style={{ backgroundImage: `url(${item})`, opacity: i === index ? 1 : 0 }}
                        />
                    ))}
                    <div className="absolute inset-0 bg-gradient-to-t from-gray-900 via-gray-900/70 to-transparent"></div>
                </div>
                <div className="relative z-10 p-6 flex flex-col h-full justify-end min-h-[200px]">
                     <div>
                        <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5 backdrop-blur-sm`}>{icon}</div>
                        <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                        <p className="text-sm text-gray-400">{description}</p>
                    </div>
                </div>
            </ModuleBase>
        );
    };

    const GreetingModule = ({ title, description, icon, ...props }) => {
        const greetingsToShow = useMemo(() => GREETINGS.sort(() => 0.5 - Math.random()).slice(0, 10), []);
    
        return (
            <ModuleBase {...props}>
                <div className="absolute inset-0 overflow-hidden p-4 h-1/2">
                    {greetingsToShow.map((g, i) => (
                        <span 
                            key={i}
                            className="absolute font-bold text-pink-400/40 animate-greeting"
                            style={{
                                top: `${Math.random() * 90}%`,
                                left: `${Math.random() * 90}%`,
                                fontSize: `${1.2 + Math.random()}rem`,
                                animationDelay: `${i * 1.5}s`
                            }}
                        >
                            {g.w}
                        </span>
                    ))}
                </div>
            <div className="relative z-10 p-6 h-full flex flex-col justify-end bg-gradient-to-t from-gray-900 via-gray-900/80 to-transparent">
                     <div>
                        <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5`}>{icon}</div>
                        <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                        <p className="text-sm text-gray-400">{description}</p>
                    </div>
                </div>
            </ModuleBase>
        );
    };
    
    const BrailleModule = ({ title, description, icon, ...props }) => {
        const randomBraille = useMemo(() => BRAILLE_CODE[Math.floor(Math.random() * BRAILLE_CODE.length)].c, []);
        return (
            <ModuleBase {...props}>
                <div className="absolute inset-0 flex items-start pt-8 justify-center">
                    <div className="text-7xl text-pink-400 animate-braille-pulse drop-shadow-glow-pink">
                        {randomBraille}
                    </div>
                </div>
                <div className="relative z-10 p-6 h-full flex flex-col justify-end bg-gradient-to-t from-gray-900 via-gray-900/80 to-transparent">
                     <div>
                        <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5 backdrop-blur-sm`}>{icon}</div>
                        <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                        <p className="text-sm text-gray-400">{description}</p>
                    </div>
                </div>
            </ModuleBase>
        );
    };
    
    const KeyboardModule = ({ title, description, icon, ...props }) => {
        const layout = useMemo(() => KEYBOARD_REGIONS[Math.floor(Math.random() * 3)], []); // QWERTY, AZERTY, QWERTZ
    
        return (
            <ModuleBase {...props}>
                <div className="absolute inset-0 flex items-center justify-center p-4 opacity-20 group-hover:opacity-40 transition-opacity">
                    <div className="flex flex-col items-center gap-1 p-2 bg-black/50 rounded-lg scale-90">
                        {layout.rows.map((row, rowIndex) => (
                            <div key={rowIndex} className="flex justify-center gap-1">
                                {row.split(" ").map((key, keyIndex) => (
                                    <div 
                                        key={keyIndex} 
                                        className="w-7 h-7 rounded bg-gray-700 flex items-center justify-center font-bold text-white shadow-md text-xs animate-key-press"
                                        style={{ animationDelay: `${Math.random() * 5}s` }}
                                    >
                                        {key[0]}
                                    </div>
                                ))}
                            </div>
                        ))}
                    </div>
                </div>
                <div className="relative z-10 p-6 h-full flex flex-col justify-end bg-gradient-to-t from-gray-900 via-gray-900/80 to-transparent">
                    <div>
                        <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5`}>{icon}</div>
                        <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                        <p className="text-sm text-gray-400">{description}</p>
                    </div>
                </div>
            </ModuleBase>
        );
    };
    
    const TldModule = ({ title, description, icon, ...props }) => {
        const tlds = useMemo(() => TLD_DATA.sort(() => 0.5 - Math.random()).slice(0, 5).map(t => t.tld), []);
        const [index, setIndex] = useState(0);
        const [isFading, setIsFading] = useState(false);
    
        useEffect(() => {
            const interval = setInterval(() => {
                setIsFading(true);
                setTimeout(() => {
                    setIndex(prev => (prev + 1) % tlds.length);
                    setIsFading(false);
                }, 300); // fade out duration
            }, 2000);
            return () => clearInterval(interval);
        }, [tlds]);
    
        return (
            <ModuleBase {...props}>
                <div className="absolute inset-0 flex items-center justify-center font-mono text-2xl text-gray-400">
                    <span>www.example</span>
                    <span 
                        className="text-blue-400 transition-opacity duration-300"
                        style={{ opacity: isFading ? 0 : 1 }}
                    >
                        {tlds[index]}
                    </span>
                </div>
                <div className="relative z-10 p-6 min-h-[200px] bg-gradient-to-t from-gray-900/80 to-transparent flex flex-col justify-end">
                     <div>
                        <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5 backdrop-blur-sm`}>{icon}</div>
                        <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                        <p className="text-sm text-gray-400">{description}</p>
                    </div>
                </div>
            </ModuleBase>
        );
    };
    
    const CalendarModule = ({ title, description, icon, ...props }) => {
        const [day, setDay] = useState(new Date().getDate());
        const month = useMemo(() => new Date().toLocaleString('default', { month: 'short' }).toUpperCase(), []);

        useEffect(() => {
            const interval = setInterval(() => {
                setDay(d => (d % 31) + 1);
            }, 100);
            return () => clearInterval(interval);
        }, []);

        return (
            <ModuleBase {...props}>
                <div className="absolute inset-0 flex items-center justify-end p-6">
                    <div className="w-44 h-44 bg-white text-black rounded-lg shadow-2xl flex flex-col overflow-hidden">
                        <div className="bg-amber-600 text-white font-bold text-center py-2">{month}</div>
                        <div className="flex-1 flex items-center justify-center text-7xl font-bold">
                            {day}
                        </div>
                    </div>
                </div>
                <div className="relative z-10 p-6 h-full flex flex-col justify-center text-left w-1/2">
                     <div>
                        <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5 backdrop-blur-sm`}>{icon}</div>
                        <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                        <p className="text-sm text-gray-400">{description}</p>
                    </div>
                </div>
            </ModuleBase>
        );
    };

    const RadarModule = ({ title, description, icon, ...props }) => (
        <ModuleBase {...props}>
            <div className="absolute inset-0 radar-bg overflow-hidden">
                <div className="radar-sweep"></div>
            </div>
            <div className="relative z-10 p-6 min-h-[200px] bg-black/30">
                <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5`}>{icon}</div>
                <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                <p className="text-sm text-gray-400">{description}</p>
            </div>
        </ModuleBase>
    );

    const MorseModule = ({ title, description, icon, ...props }) => (
        <ModuleBase {...props}>
            <div className="absolute inset-0 flex items-center justify-center overflow-hidden">
                <div className="morse-light"></div>
            </div>
            <div className="relative z-10 p-6 min-h-[200px] bg-black/30">
                <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5`}>{icon}</div>
                <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                <p className="text-sm text-gray-400">{description}</p>
            </div>
        </ModuleBase>
    );

    const PeriodicModule = ({ title, description, icon, ...props }) => {
        const randomElements = useMemo(() => 
            [...Array(36)].map(() => {
                if (Math.random() > 0.7) {
                    return ELEMENTS[Math.floor(Math.random() * ELEMENTS.length)].s;
                }
                return Math.random() > 0.5 ? '?' : '';
            }), []);
    
        return (
            <ModuleBase {...props}>
                <div className="absolute inset-0 opacity-25 group-hover:opacity-60 transition-opacity duration-500 overflow-hidden p-2">
                    <div className="periodic-grid">
                        {randomElements.map((content, i) => (
                            <div key={i} className="periodic-cell">
                                <span 
                                    className="periodic-content"
                                    style={{ animationDelay: `${Math.random() * 10}s` }}
                                >
                                    {content}
                                </span>
                            </div>
                        ))}
                    </div>
                </div>
                <div className="relative z-10 p-6 min-h-[200px]">
                    <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5`}>{icon}</div>
                    <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                    <p className="text-sm text-gray-400">{description}</p>
                </div>
            </ModuleBase>
        );
    };

    const ScrollingNotesModule = ({ title, description, icon, ...props }) => (
        <ModuleBase {...props}>
            <div className="absolute inset-0 music-staff-bg overflow-hidden">
                <div className="music-note text-purple-300" style={{ top: '30%', animationDelay: '0s' }}>♪</div>
                <div className="music-note text-purple-300" style={{ top: '50%', animationDelay: '1s' }}>♫</div>
                <div className="music-note text-purple-300" style={{ top: '40%', animationDelay: '2.5s' }}>♪</div>
            </div>
            <div className="relative z-10 p-6 min-h-[200px] bg-black/50 backdrop-blur-sm h-full">
                <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5`}>{icon}</div>
                <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                <p className="text-sm text-gray-400">{description}</p>
            </div>
        </ModuleBase>
    );

    const ScrollingNamesModule = ({ title, description, icon, items, itemIcon, ...props }) => (
        <ModuleBase {...props}>
            <div className="absolute inset-0 overflow-hidden mask-gradient-vertical">
                <div className="animate-scroll-vertical-slow">
                    <div className="flex flex-col gap-2 p-4">
                        {items.map((item, i) => <div key={i} className={`text-${props.color}-400/30 font-serif font-bold text-2xl flex items-center gap-3`}><span className="opacity-50">{itemIcon}</span>{item}</div>)}
                    </div>
                    <div className="flex flex-col gap-2 p-4" aria-hidden="true">
                        {items.map((item, i) => <div key={i} className={`text-${props.color}-400/30 font-serif font-bold text-2xl flex items-center gap-3`}><span className="opacity-50">{itemIcon}</span>{item}</div>)}
                    </div>
                </div>
            </div>
            <div className="relative z-10 p-6 h-full flex flex-col justify-end bg-gradient-to-t from-gray-900 via-gray-900/80 to-transparent">
                 <div>
                    <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5`}>{icon}</div>
                    <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                    <p className="text-sm text-gray-400">{description}</p>
                </div>
            </div>
        </ModuleBase>
    );

    const AllianceModule = ({ title, description, icon, ...props }) => {
        const allianceMembers = useMemo(() => ALLIANCES.find(a => a.id === "NATO").members, []);
        const memberCountries = useMemo(() => FLAGS.filter(f => allianceMembers.includes(f.n)), [allianceMembers]);
        const flagsToShow = useMemo(() => memberCountries.sort(() => 0.5 - Math.random()).slice(0, 12), [memberCountries]);

        return (
            <ModuleBase {...props}>
                <div className="absolute inset-0 overflow-hidden p-4 grid grid-cols-4 grid-rows-3 gap-2 opacity-20 group-hover:opacity-40 transition-opacity">
                    {flagsToShow.map((flag, i) => (
                        <img
                            key={i}
                            src={`https://flagcdn.com/w80/${flag.code}.png`}
                            alt=""
                            className="w-full h-full object-cover rounded-sm animate-fade-in-out"
                            style={{ animationDelay: `${i * 0.7}s` }}
                        />
                    ))}
                </div>
                <div className="relative z-10 p-6 h-full flex flex-col justify-end bg-gradient-to-t from-gray-900 via-gray-900/80 to-transparent">
                     <div>
                        <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5`}>{icon}</div>
                        <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                        <p className="text-sm text-gray-400">{description}</p>
                    </div>
                </div>
            </ModuleBase>
        );
    };

    const MapModule = ({ title, description, icon, ...props }) => (
        <ModuleBase {...props}>
            <div 
                className="absolute inset-0 bg-cover bg-center opacity-20 group-hover:opacity-30 transition-opacity animate-pan"
                style={{ backgroundImage: `url('https://upload.wikimedia.org/wikipedia/commons/2/23/Ancient_map_of_the_world%2C_by_Hendrik_Hondius%2C_1630.jpg')` }}
            ></div>
            <div className="absolute inset-0 overflow-hidden">
                {[
                    { Icon: Castle, size: 'w-12 h-12', pos: { top: '10%', left: '15%' }, delay: '0s' },
                    { Icon: Crown, size: 'w-10 h-10', pos: { top: '50%', left: '70%' }, delay: '5s' },
                    { Icon: ScrollText, size: 'w-10 h-10', pos: { top: '70%', left: '20%' }, delay: '10s' },
                ].map(({ Icon, size, pos, delay }, i) => (
                    <Icon 
                        key={i}
                        className={`absolute text-yellow-200/20 animate-float ${size}`}
                        style={{ ...pos, animationDelay: delay, animationDuration: `${12 + Math.random() * 6}s` }}
                    />
                ))}
            </div>
            <div className="absolute inset-0 bg-radial-gradient-c"></div>
            <div className="relative z-10 p-6 h-full flex flex-col justify-end bg-gradient-to-t from-gray-900 via-gray-900/80 to-transparent">
                 <div>
                    <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5 backdrop-blur-sm`}>{icon}</div>
                    <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                    <p className="text-sm text-gray-400">{description}</p>
                </div>
            </div>
        </ModuleBase>
    );

    const FallingItemsModule = ({ title, description, icon, items, ...props }) => (
        <ModuleBase {...props}>
            <div className="absolute inset-0 overflow-hidden">
                {items.map((item, i) => (
                    <img 
                        key={i}
                        src={item}
                        alt=""
                        className="absolute w-8 h-auto animate-fall"
                        style={{
                            left: `${Math.random() * 90}%`,
                            animationDelay: `${Math.random() * 10}s`,
                            animationDuration: `${5 + Math.random() * 5}s`
                        }}
                    />
                ))}
            </div>
            <div className="relative z-10 p-6 flex flex-col h-full justify-end min-h-[200px] bg-gradient-to-t from-gray-900 via-gray-900/80 to-transparent">
                 <div>
                    <div className={`p-3 mb-4 inline-block bg-${props.color}-500/10 rounded-lg text-${props.color}-400 border border-white/5 backdrop-blur-sm`}>{icon}</div>
                    <h3 className="text-xl font-bold mb-1 text-white">{title}</h3>
                    <p className="text-sm text-gray-400">{description}</p>
                </div>
            </div>
        </ModuleBase>
    );

    return (
        <div className="min-h-screen flex flex-col bg-gray-900 text-white font-sans">
             <style>{`
                @import url('https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;700&family=Inter:wght@400;600;800&family=Cinzel:wght@400;700&display=swap');
                .font-mono { font-family: 'JetBrains Mono', monospace; }
                .font-sans { font-family: 'Inter', sans-serif; }
                .font-serif { font-family: 'Cinzel', serif; }
                .animate-wiggle { animation: wiggle 0.3s ease-in-out; }
                @keyframes wiggle { 0%, 100% { transform: rotate(0deg); } 25% { transform: rotate(-5deg); } 75% { transform: rotate(5deg); } }
                .animate-fade-in { animation: fadeIn 0.7s ease-out; }
                @keyframes float {
                    0% { transform: translateY(0px) translateX(0px) rotate(0deg); opacity: 0.8; }
                    25% { transform: translateY(-10px) translateX(5px) rotate(5deg); opacity: 1; }
                    50% { transform: translateY(0px) translateX(-5px) rotate(0deg); opacity: 0.8; }
                    75% { transform: translateY(10px) translateX(5px) rotate(-5deg); opacity: 0.7; }
                    100% { transform: translateY(0px) translateX(0px) rotate(0deg); opacity: 0.8; }
                }
                .animate-float { animation: float 15s ease-in-out infinite; }
                @keyframes pulse-dot {
                    0%, 100% { opacity: 0.2; }
                    50% { opacity: 0.8; }
                }
                .animate-pulse-dot { animation: pulse-dot 4s infinite; }
                @keyframes globe-pulse {
                    0%, 100% { transform: scale(0.1); opacity: 0; }
                    50% { transform: scale(1); opacity: 1; }
                    80% { transform: scale(0.9); opacity: 0.5; }
                }
                .animate-globe-pulse { animation: globe-pulse 3s infinite ease-out; }
                @keyframes dial-press {
                    0%, 10%, 100% { background-color: #374151; transform: scale(1); } /* gray-700 */
                    5% { background-color: #2563eb; transform: scale(0.95); } /* blue-600 */
                }
                .animate-dial-press { animation: dial-press 8s infinite; }
                @keyframes fadeIn { from { opacity: 0; transform: translateY(10px); } to { opacity: 1; transform: translateY(0); } }
                
                @keyframes blob {
                    0% { transform: translate(0px, 0px) scale(1); }
                    33% { transform: translate(30px, -50px) scale(1.1); }
                    66% { transform: translate(-20px, 20px) scale(0.9); }
                    100% { transform: translate(0px, 0px) scale(1); }
                }
                .animate-blob { animation: blob 7s infinite; }
                .animation-delay-2000 { animation-delay: 2s; }
                .animation-delay-4000 { animation-delay: 4s; }

                @keyframes scroll-vertical { 0% { transform: translateY(0); } 100% { transform: translateY(-50%); } }
                .animate-scroll-vertical { animation: scroll-vertical 60s linear infinite; height: 200%; }

                @keyframes scroll-vertical-slow { 0% { transform: translateY(0); } 100% { transform: translateY(-50%); } }
                .animate-scroll-vertical-slow { animation: scroll-vertical-slow 120s linear infinite; height: 200%; }
                .animate-scroll-vertical { animation: scroll-vertical 60s linear infinite; height: 200%; }

                @keyframes typing-effect { from { width: 0; } to { width: 100%; } }
                @keyframes blink-caret { from, to { border-color: transparent; } 50% { border-color: currentColor; } }
                .typing-effect-container > p { overflow: hidden; white-space: nowrap; border-right: .15em solid; animation: typing-effect 3s steps(30, end) infinite, blink-caret .75s step-end infinite; }

                @keyframes greeting-float {
                    0%, 100% { opacity: 0; transform: translateY(20px); }
                    20%, 80% { opacity: 1; transform: translateY(0); }
                }
                .animate-greeting { animation: greeting-float 5s ease-in-out infinite; }

                @keyframes braille-pulse {
                    0%, 100% { transform: scale(1); opacity: 1; }
                    50% { transform: scale(1.05); opacity: 0.8; }
                }
                .animate-braille-pulse { animation: braille-pulse 4s ease-in-out infinite; }
                .drop-shadow-glow-pink { filter: drop-shadow(0 0 8px rgba(236, 72, 153, 0.7)); }

                @keyframes key-press {
                    0%, 10%, 100% { background-color: #374151; transform: translateY(0); } /* gray-700 */
                    5% { background-color: #0d9488; transform: translateY(-2px); } /* teal-600 */
                }
                .animate-key-press { animation: key-press 5s infinite; }

                @keyframes content-pop {
                    0%, 10%, 100% { opacity: 0; transform: scale(0.5); }
                    5% { opacity: 1; transform: scale(1); }
                }
                .periodic-content {
                    position: absolute; inset: 0; display: flex; align-items: center; justify-content: center; font-family: 'JetBrains Mono', monospace; font-weight: bold; color: #4ade80; opacity: 0; animation: content-pop 10s infinite;
                }

                @keyframes radar-sweep { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }
                .radar-bg { background: repeating-radial-gradient(circle at center, rgba(100, 116, 139, 0.2) 0, rgba(100, 116, 139, 0.2) 1px, transparent 1px, transparent 20%), #0f172a; }
                .radar-sweep { position: absolute; top: -50%; left: -50%; width: 200%; height: 200%; background: conic-gradient(from 0deg, transparent 0%, transparent 75%, rgba(100, 116, 139, 0.5) 99%, transparent 100%); animation: radar-sweep 4s linear infinite; }

                @keyframes morse-sos {
                    /* S */ 0%, 2% { opacity: 1; box-shadow: 0 0 30px #f0e68c; } 3% { opacity: 0.2; box-shadow: none; }
                    4%, 6% { opacity: 1; box-shadow: 0 0 30px #f0e68c; } 7% { opacity: 0.2; box-shadow: none; }
                    8%, 10% { opacity: 1; box-shadow: 0 0 30px #f0e68c; } 11% { opacity: 0.2; box-shadow: none; }
                    /* space */ 11.1%, 13% { opacity: 0.2; box-shadow: none; }
                    /* O */ 14%, 18% { opacity: 1; box-shadow: 0 0 30px #f0e68c; } 19% { opacity: 0.2; box-shadow: none; }
                    20%, 24% { opacity: 1; box-shadow: 0 0 30px #f0e68c; } 25% { opacity: 0.2; box-shadow: none; }
                    26%, 30% { opacity: 1; box-shadow: 0 0 30px #f0e68c; } 31% { opacity: 0.2; box-shadow: none; }
                    /* space */ 31.1%, 33% { opacity: 0.2; box-shadow: none; }
                    /* S */ 34%, 36% { opacity: 1; box-shadow: 0 0 30px #f0e68c; } 37% { opacity: 0.2; box-shadow: none; }
                    38%, 40% { opacity: 1; box-shadow: 0 0 30px #f0e68c; } 41% { opacity: 0.2; box-shadow: none; }
                    42%, 44% { opacity: 1; box-shadow: 0 0 30px #f0e68c; } 45% { opacity: 0.2; box-shadow: none; }
                    /* long pause */ 45.1%, 100% { opacity: 0.2; box-shadow: none; }
                }
                .morse-light { width: 50px; height: 50px; background: #f0e68c; border-radius: 50%; opacity: 0.2; animation: morse-sos 5s infinite; }

                .periodic-grid { display: grid; grid-template-columns: repeat(9, 1fr); gap: 2px; }
                .periodic-cell { 
                    width: 100%; 
                    padding-bottom: 100%; 
                    background: rgba(34, 197, 94, 0.05); 
                    border: 1px solid rgba(34, 197, 94, 0.1); 
                    border-radius: 2px;
                    position: relative;
                }

                @keyframes scroll-notes { from { left: 110%; } to { left: -10%; } }
                .music-staff-bg { background: repeating-linear-gradient( to bottom, transparent, transparent 19.5%, rgba(255, 255, 255, 0.1) 20%, rgba(255, 255, 255, 0.1) 20.5%, transparent 21% ); }
                .music-note { position: absolute; font-size: 2rem; animation: scroll-notes 5s linear infinite; }

                @keyframes fall {
                    from { top: -10%; transform: rotate(0deg); }
                    to { top: 110%; transform: rotate(720deg); }
                }
                .animate-fall { animation-name: fall; animation-timing-function: linear; animation-iteration-count: infinite; }

                @keyframes pan { 0% { background-position: 0% 50%; } 50% { background-position: 100% 50%; } 100% { background-position: 0% 50%; } }
                .animate-pan { animation: pan 40s ease-in-out infinite; }

                .bg-radial-gradient-c { background-image: radial-gradient(circle, transparent, black 80%); }

                @keyframes fade-in-out { 0%, 100% { opacity: 0; } 50% { opacity: 1; } }
                .animate-fade-in-out { animation: fade-in-out 8s infinite; }

                .mask-gradient-vertical { mask-image: linear-gradient(to bottom, transparent, black 20%, black 80%, transparent); -webkit-mask-image: linear-gradient(to bottom, transparent, black 20%, black 80%, transparent); }


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
                <div className="max-w-6xl mx-auto px-4 h-16 flex items-center justify-between">
                    <div className="flex items-center gap-3 cursor-pointer" onClick={() => setView("HOME")}>
                        <Brain className="text-purple-500" size={24} />
                        <h1 className="font-bold text-lg md:text-xl tracking-tight hidden md:block">
                            {view === "HOME" ? (<span>idk why i made this lol</span>) : (<span>{GAME_TITLES[view]}</span>)}
                        </h1>
                    </div>
                    
                    <div className="flex items-center gap-4">
                        <div className="flex items-center gap-2 bg-gray-800 px-3 py-1 rounded-full border border-purple-500/30">
                            <Trophy className="text-yellow-400" size={16} />
                            <span className="font-bold font-mono text-yellow-400">{points.toLocaleString()}</span>
                        </div>

                        {view !== "HOME" && (
                            <button onClick={() => setView("HOME")} className="flex items-center gap-1 text-sm text-gray-400 hover:text-white transition-colors">
                                <ChevronLeft size={16} /> Back to Menu
                            </button>
                        )}
                    </div>
                </div>
            </header>

            <main className="flex-1 p-4 md:p-8 overflow-y-auto">
                {view === "HOME" ? (
                    (() => {
                        const geoIds = ["GEO", "FLAGS", "CALLING", "CURRENCY", "DISH", "POP"];
                        const historyIds = ["PRESIDENTS", "MONARCHS", "ALLIANCE", "EMPIRES", "ON_THIS_DAY"];
                        const scienceIds = ["ELEMENTS", "PI", "STARS", "MUSIC"];
                        const techIds = ["CODE", "BINARY", "TLD", "KEYBOARD_REGION"];
                        const langIds = ["LANG", "NATO", "MORSE", "BRAILLE"];

                        const allCompleted = completedGames.length === ALL_GAME_IDS.length;
                        const completionPercent = (completedGames.length / ALL_GAME_IDS.length) * 100;

                        return (
                            <div className="max-w-6xl mx-auto animate-fade-in">
                                <div className="text-center mb-16 relative">
                                    <div className="absolute -top-20 -left-20 w-64 h-64 bg-purple-500 rounded-full mix-blend-lighten filter blur-3xl opacity-30 animate-blob"></div>
                                    <div className="absolute -top-20 -right-20 w-64 h-64 bg-yellow-500 rounded-full mix-blend-lighten filter blur-3xl opacity-30 animate-blob animation-delay-2000"></div>
                                    <div className="absolute top-0 -right-40 w-64 h-64 bg-pink-500 rounded-full mix-blend-lighten filter blur-3xl opacity-30 animate-blob animation-delay-4000"></div>
                                    
                                    <h2 className="text-4xl md:text-6xl font-black mb-4 text-white">Useless Knowledge Trainer</h2>
                                    <p className="text-gray-400 text-lg max-w-2xl mx-auto">A curated collection of quizzes to master wonderfully useless information, from vexillology to the first 100 digits of pi.</p>
                                    
                                    <div className="mt-8 flex justify-center items-center gap-4">
                                        <div className="w-full max-w-md bg-gray-800/50 rounded-full h-3 border border-gray-700 overflow-hidden">
                                            <div className="h-full bg-gradient-to-r from-purple-500 to-pink-500 transition-all duration-500" style={{ width: `${completionPercent}%` }}></div>
                                        </div>
                                        <span className="font-mono text-gray-300">{completedGames.length}/{ALL_GAME_IDS.length}</span>
                                    </div>
                                </div>

                                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
                                    
                                    {allCompleted && (
                                        <div className="lg:col-span-4 mb-8 p-6 bg-gradient-to-r from-yellow-400 via-amber-500 to-orange-500 rounded-xl text-center shadow-lg border-2 border-yellow-300">
                                            <Trophy className="mx-auto text-white mb-4 drop-shadow-lg" size={48} />
                                            <h2 className="text-3xl font-bold text-white">Congratulations!</h2>
                                            <p className="text-yellow-100 mt-2">You have mastered all modules. Your knowledge of wonderfully useless things is complete.</p>
                                        </div>
                                    )}

                                    {/* GEOGRAPHY & TRAVEL */}
                                    <SectionHeader title="Geography & Travel" color="blue" completed={geoIds.filter(id => completedGames.includes(id)).length} total={geoIds.length} />
                                    <GlobeModule id="GEO" onClick={() => setView("GEO")} isComplete={completedGames.includes("GEO")} color="blue" title="Capital Cities" description="Test your knowledge of world capitals." icon={<Globe size={24} />} span={2}/>
                                    <FallingItemsModule id="FLAGS" onClick={() => setView("FLAGS")} isComplete={completedGames.includes("FLAGS")} color="yellow" title="Vexillology" description="Recognize national flags." icon={<Flag size={24} />} items={FLAGS.slice(0, 15).map(f => `https://flagcdn.com/w160/${f.code}.png`)} span={2}/>
                                    <DialerModule id="CALLING" onClick={() => setView("CALLING")} isComplete={completedGames.includes("CALLING")} color="blue" title="Calling Codes" description="Identify international calling codes." icon={<Phone size={24} />} />
                                    <FallingSymbolsModule id="CURRENCY" onClick={() => setView("CURRENCY")} isComplete={completedGames.includes("CURRENCY")} color="emerald" title="Currencies" description="Match currencies to countries." icon={<Coins size={24} />} items={['$', '€', '£', '¥', '₩', '₹', '₽']} />
                                    <FallingSymbolsModule id="DISH" onClick={() => setView("DISH")} isComplete={completedGames.includes("DISH")} color="orange" title="National Dishes" description="Connect dishes to their origin." icon={<Utensils size={24} />} items={['🌿', '🍅', '🍞', '🧀', '🌶️', '🍋', '🥩', '🍚']} />
                                    <PulsingDotsModule id="POP" onClick={() => setView("POP")} isComplete={completedGames.includes("POP")} color="indigo" title="Population Guesser" description="Estimate country populations." icon={<Users size={24} />} />

                                    {/* HISTORY & POWER */}
                                    <SectionHeader title="History & Power" color="red" completed={historyIds.filter(id => completedGames.includes(id)).length} total={historyIds.length} />
                                    <ScrollingNamesModule id="PRESIDENTS" onClick={() => setView("PRESIDENTS")} isComplete={completedGames.includes("PRESIDENTS")} color="red" title="US Presidents" description="Recall the order of US Presidents." icon={<Flag size={24} />} items={US_PRESIDENTS.map(p => p.name)} />
                                    <ScrollingNamesModule id="MONARCHS" onClick={() => setView("MONARCHS")} isComplete={completedGames.includes("MONARCHS")} color="purple" title="Royal Lineage" description="Trace the British line of succession." icon={<Crown size={24} />} items={BRITISH_MONARCHS} itemIcon={<Crown size={16} />} />
                                    <AllianceModule id="ALLIANCE" onClick={() => setView("ALLIANCE")} isComplete={completedGames.includes("ALLIANCE")} color="cyan" title="Alliances" description="Name members of NATO, G7, etc." icon={<Shield size={24} />} />
                                    <CalendarModule id="ON_THIS_DAY" onClick={() => setView("ON_THIS_DAY")} isComplete={completedGames.includes("ON_THIS_DAY")} color="amber" title="On This Day" description="Explore historical events by date." icon={<CalendarDays size={24} />} span={3} />
                                    <MapModule id="EMPIRES" onClick={() => setView("EMPIRES")} isComplete={completedGames.includes("EMPIRES")} color="yellow" title="Empires" description="Find modern countries in old empires." icon={<Castle size={24} />} />

                                    {/* SCIENCE & NATURE */}
                                    <SectionHeader title="Science & Nature" color="green" completed={scienceIds.filter(id => completedGames.includes(id)).length} total={scienceIds.length} />
                                    <PeriodicModule id="ELEMENTS" onClick={() => setView("ELEMENTS")} isComplete={completedGames.includes("ELEMENTS")} color="green" title="The Periodic Table" description="Match symbols to element names." icon={<FlaskConical size={24} />} />
                                    <MatrixModule id="PI" onClick={() => setView("PI")} isComplete={completedGames.includes("PI")} color="purple" title="100 Digits of Pi" description="Recite the famous constant." icon={<span className="font-mono font-bold text-xl">π</span>} data={PI_100} />
                                    <StarfieldModule id="STARS" onClick={() => setView("STARS")} isComplete={completedGames.includes("STARS")} color="yellow" title="Constellations" description="Identify major star patterns." icon={<Star size={24} />} span={2} />
                                    <ScrollingNotesModule id="MUSIC" onClick={() => setView("MUSIC")} isComplete={completedGames.includes("MUSIC")} color="purple" title="Music Notes" description="Identify notes on a staff." icon={<Music size={24} />} />
                                    {/* COMPUTING & TECH */}
                                    <SectionHeader title="Computing & Tech" color="emerald" completed={techIds.filter(id => completedGames.includes(id)).length} total={techIds.length} />
                                    <TypingModule id="CODE" onClick={() => setView("CODE")} isComplete={completedGames.includes("CODE")} color="emerald" title="Hello World" description="Code in 30+ languages." icon={<Terminal size={24} />} code={`console.log("Hello, World!");`} span={2} />
                                    <MatrixModule id="BINARY" onClick={() => setView("BINARY")} isComplete={completedGames.includes("BINARY")} color="green" title="ASCII Binary" description="Convert characters to 8-bit." icon={<Cpu size={24} />} data={ASCII_DATA.map(d => d.bin).join('')} />
                                    <TldModule id="TLD" onClick={() => setView("TLD")} isComplete={completedGames.includes("TLD")} color="blue" title="Domain Names" description="Match ccTLDs to their country." icon={<Wifi size={24} />} />
                                    <KeyboardModule id="KEYBOARD_REGION" onClick={() => setView("KEYBOARD_REGION")} isComplete={completedGames.includes("KEYBOARD_REGION")} color="teal" title="Keyboard Regions" description="Identify a keyboard's region." icon={<Keyboard size={24} />} />

                                    {/* LANGUAGE & CIPHER */}
                                    <SectionHeader title="Language & Cipher" color="pink" completed={langIds.filter(id => completedGames.includes(id)).length} total={langIds.length} />
                                    <GreetingModule id="LANG" onClick={() => setView("LANG")} isComplete={completedGames.includes("LANG")} color="pink" title="Hello Person" description="Say 'Hello' in dozens of languages." icon={<Languages size={24} />} />
                                    <RadarModule id="NATO" onClick={() => setView("NATO")} isComplete={completedGames.includes("NATO")} color="slate" title="NATO Phonetic" description="Master Alpha, Bravo, Charlie..." icon={<Radio size={24} />} />
                                    <MorseModule id="MORSE" onClick={() => setView("MORSE")} isComplete={completedGames.includes("MORSE")} color="blue" title="Morse Code" description="Translate letters to dots and dashes." icon={<Hash size={24} />} />
                                    <BrailleModule id="BRAILLE" onClick={() => setView("BRAILLE")} isComplete={completedGames.includes("BRAILLE")} color="pink" title="Braille" description="Recognize tactile alphabet letters." icon={<Glasses size={24} />} />
                                </div>
                            </div>
                        )
                    })()
                ): (
                    renderGame()
                )}
            </main>
        </div>
    );
}
