//usr/bin/clang++ "$0" -g -std=c++17 -Ofast -o /tmp/eldrow && exec time /tmp/eldrow "$@" ; exit

#include <iostream>
#include <unordered_map>
#include <vector>
using namespace std;

const char* WORDLIST[] = {"ABACK","ABASE","ABATE","ABBEY","ABBOT","ABHOR","ABIDE","ABLED","ABODE","ABORT","ABOUT","ABOVE","ABUSE","ABYSS","ACORN","ACRID","ACTOR","ACUTE","ADAGE","ADAPT","ADEPT","ADMIN","ADMIT","ADOBE","ADOPT","ADORE","ADORN","ADULT","AFFIX","AFIRE","AFOOT","AFOUL","AFTER","AGAIN","AGAPE","AGATE","AGENT","AGILE","AGING","AGLOW","AGONY","AGORA","AGREE","AHEAD","AIDER","AISLE","ALARM","ALBUM","ALERT","ALGAE","ALIBI","ALIEN","ALIGN","ALIKE","ALIVE","ALLAY","ALLEY","ALLOT","ALLOW","ALLOY","ALOFT","ALONE","ALONG","ALOOF","ALOUD","ALPHA","ALTAR","ALTER","AMASS","AMAZE","AMBER","AMBLE","AMEND","AMISS","AMITY","AMONG","AMPLE","AMPLY","AMUSE","ANGEL","ANGER","ANGLE","ANGRY","ANGST","ANIME","ANKLE","ANNEX","ANNOY","ANNUL","ANODE","ANTIC","ANVIL","AORTA","APART","APHID","APING","APNEA","APPLE","APPLY","APRON","APTLY","ARBOR","ARDOR","ARENA","ARGUE","ARISE","ARMOR","AROMA","AROSE","ARRAY","ARROW","ARSON","ARTSY","ASCOT","ASHEN","ASIDE","ASKEW","ASSAY","ASSET","ATOLL","ATONE","ATTIC","AUDIO","AUDIT","AUGUR","AUNTY","AVAIL","AVERT","AVIAN","AVOID","AWAIT","AWAKE","AWARD","AWARE","AWASH","AWFUL","AWOKE","AXIAL","AXIOM","AXION","AZURE","BACON","BADGE","BADLY","BAGEL","BAGGY","BAKER","BALER","BALMY","BANAL","BANJO","BARGE","BARON","BASAL","BASIC","BASIL","BASIN","BASIS","BASTE","BATCH","BATHE","BATON","BATTY","BAWDY","BAYOU","BEACH","BEADY","BEARD","BEAST","BEECH","BEEFY","BEFIT","BEGAN","BEGAT","BEGET","BEGIN","BEGUN","BEING","BELCH","BELIE","BELLE","BELLY","BELOW","BENCH","BERET","BERRY","BERTH","BESET","BETEL","BEVEL","BEZEL","BIBLE","BICEP","BIDDY","BIGOT","BILGE","BILLY","BINGE","BINGO","BIOME","BIRCH","BIRTH","BISON","BITTY","BLACK","BLADE","BLAME","BLAND","BLANK","BLARE","BLAST","BLAZE","BLEAK","BLEAT","BLEED","BLEEP","BLEND","BLESS","BLIMP","BLIND","BLINK","BLISS","BLITZ","BLOAT","BLOCK","BLOKE","BLOND","BLOOD","BLOOM","BLOWN","BLUER","BLUFF","BLUNT","BLURB","BLURT","BLUSH","BOARD","BOAST","BOBBY","BONEY","BONGO","BONUS","BOOBY","BOOST","BOOTH","BOOTY","BOOZE","BOOZY","BORAX","BORNE","BOSOM","BOSSY","BOTCH","BOUGH","BOULE","BOUND","BOWEL","BOXER","BRACE","BRAID","BRAIN","BRAKE","BRAND","BRASH","BRASS","BRAVE","BRAVO","BRAWL","BRAWN","BREAD","BREAK","BREED","BRIAR","BRIBE","BRICK","BRIDE","BRIEF","BRINE","BRING","BRINK","BRINY","BRISK","BROAD","BROIL","BROKE","BROOD","BROOK","BROOM","BROTH","BROWN","BRUNT","BRUSH","BRUTE","BUDDY","BUDGE","BUGGY","BUGLE","BUILD","BUILT","BULGE","BULKY","BULLY","BUNCH","BUNNY","BURLY","BURNT","BURST","BUSED","BUSHY","BUTCH","BUTTE","BUXOM","BUYER","BYLAW","CABAL","CABBY","CABIN","CABLE","CACAO","CACHE","CACTI","CADDY","CADET","CAGEY","CAIRN","CAMEL","CAMEO","CANAL","CANDY","CANNY","CANOE","CANON","CAPER","CAPUT","CARAT","CARGO","CAROL","CARRY","CARVE","CASTE","CATCH","CATER","CATTY","CAULK","CAUSE","CAVIL","CEASE","CEDAR","CELLO","CHAFE","CHAFF","CHAIN","CHAIR","CHALK","CHAMP","CHANT","CHAOS","CHARD","CHARM","CHART","CHASE","CHASM","CHEAP","CHEAT","CHECK","CHEEK","CHEER","CHESS","CHEST","CHICK","CHIDE","CHIEF","CHILD","CHILI","CHILL","CHIME","CHINA","CHIRP","CHOCK","CHOIR","CHOKE","CHORD","CHORE","CHOSE","CHUCK","CHUMP","CHUNK","CHURN","CHUTE","CIDER","CIGAR","CINCH","CIRCA","CIVIC","CIVIL","CLACK","CLAIM","CLAMP","CLANG","CLANK","CLASH","CLASP","CLASS","CLEAN","CLEAR","CLEAT","CLEFT","CLERK","CLICK","CLIFF","CLIMB","CLING","CLINK","CLOAK","CLOCK","CLONE","CLOSE","CLOTH","CLOUD","CLOUT","CLOVE","CLOWN","CLUCK","CLUED","CLUMP","CLUNG","COACH","COAST","COBRA","COCOA","COLON","COLOR","COMET","COMFY","COMIC","COMMA","CONCH","CONDO","CONIC","COPSE","CORAL","CORER","CORNY","COUCH","COUGH","COULD","COUNT","COUPE","COURT","COVEN","COVER","COVET","COVEY","COWER","COYLY","CRACK","CRAFT","CRAMP","CRANE","CRANK","CRASH","CRASS","CRATE","CRAVE","CRAWL","CRAZE","CRAZY","CREAK","CREAM","CREDO","CREED","CREEK","CREEP","CREME","CREPE","CREPT","CRESS","CREST","CRICK","CRIED","CRIER","CRIME","CRIMP","CRISP","CROAK","CROCK","CRONE","CRONY","CROOK","CROSS","CROUP","CROWD","CROWN","CRUDE","CRUEL","CRUMB","CRUMP","CRUSH","CRUST","CRYPT","CUBIC","CUMIN","CURIO","CURLY","CURRY","CURSE","CURVE","CURVY","CUTIE","CYBER","CYCLE","CYNIC","DADDY","DAILY","DAIRY","DAISY","DALLY","DANCE","DANDY","DATUM","DAUNT","DEALT","DEATH","DEBAR","DEBIT","DEBUG","DEBUT","DECAL","DECAY","DECOR","DECOY","DECRY","DEFER","DEIGN","DEITY","DELAY","DELTA","DELVE","DEMON","DEMUR","DENIM","DENSE","DEPOT","DEPTH","DERBY","DETER","DETOX","DEUCE","DEVIL","DIARY","DICEY","DIGIT","DILLY","DIMLY","DINER","DINGO","DINGY","DIODE","DIRGE","DIRTY","DISCO","DITCH","DITTO","DITTY","DIVER","DIZZY","DODGE","DODGY","DOGMA","DOING","DOLLY","DONOR","DONUT","DOPEY","DOUBT","DOUGH","DOWDY","DOWEL","DOWNY","DOWRY","DOZEN","DRAFT","DRAIN","DRAKE","DRAMA","DRANK","DRAPE","DRAWL","DRAWN","DREAD","DREAM","DRESS","DRIED","DRIER","DRIFT","DRILL","DRINK","DRIVE","DROIT","DROLL","DRONE","DROOL","DROOP","DROSS","DROVE","DROWN","DRUID","DRUNK","DRYER","DRYLY","DUCHY","DULLY","DUMMY","DUMPY","DUNCE","DUSKY","DUSTY","DUTCH","DUVET","DWARF","DWELL","DWELT","DYING","EAGER","EAGLE","EARLY","EARTH","EASEL","EATEN","EATER","EBONY","ECLAT","EDICT","EDIFY","EERIE","EGRET","EIGHT","EJECT","EKING","ELATE","ELBOW","ELDER","ELECT","ELEGY","ELFIN","ELIDE","ELITE","ELOPE","ELUDE","EMAIL","EMBED","EMBER","EMCEE","EMPTY","ENACT","ENDOW","ENEMA","ENEMY","ENJOY","ENNUI","ENSUE","ENTER","ENTRY","ENVOY","EPOCH","EPOXY","EQUAL","EQUIP","ERASE","ERECT","ERODE","ERROR","ERUPT","ESSAY","ESTER","ETHER","ETHIC","ETHOS","ETUDE","EVADE","EVENT","EVERY","EVICT","EVOKE","EXACT","EXALT","EXCEL","EXERT","EXILE","EXIST","EXPEL","EXTOL","EXTRA","EXULT","EYING","FABLE","FACET","FAINT","FAIRY","FAITH","FALSE","FANCY","FANNY","FARCE","FATAL","FATTY","FAULT","FAUNA","FAVOR","FEAST","FECAL","FEIGN","FELLA","FELON","FEMME","FEMUR","FENCE","FERAL","FERRY","FETAL","FETCH","FETID","FETUS","FEVER","FEWER","FIBER","FIBRE","FICUS","FIELD","FIEND","FIERY","FIFTH","FIFTY","FIGHT","FILER","FILET","FILLY","FILMY","FILTH","FINAL","FINCH","FINER","FIRST","FISHY","FIXER","FIZZY","FJORD","FLACK","FLAIL","FLAIR","FLAKE","FLAKY","FLAME","FLANK","FLARE","FLASH","FLASK","FLECK","FLEET","FLESH","FLICK","FLIER","FLING","FLINT","FLIRT","FLOAT","FLOCK","FLOOD","FLOOR","FLORA","FLOSS","FLOUR","FLOUT","FLOWN","FLUFF","FLUID","FLUKE","FLUME","FLUNG","FLUNK","FLUSH","FLUTE","FLYER","FOAMY","FOCAL","FOCUS","FOGGY","FOIST","FOLIO","FOLLY","FORAY","FORCE","FORGE","FORGO","FORTE","FORTH","FORTY","FORUM","FOUND","FOYER","FRAIL","FRAME","FRANK","FRAUD","FREAK","FREED","FREER","FRESH","FRIAR","FRIED","FRILL","FRISK","FRITZ","FROCK","FROND","FRONT","FROST","FROTH","FROWN","FROZE","FRUIT","FUDGE","FUGUE","FULLY","FUNGI","FUNKY","FUNNY","FUROR","FURRY","FUSSY","FUZZY","GAFFE","GAILY","GAMER","GAMMA","GAMUT","GASSY","GAUDY","GAUGE","GAUNT","GAUZE","GAVEL","GAWKY","GAYER","GAYLY","GAZER","GECKO","GEEKY","GEESE","GENIE","GENRE","GHOST","GHOUL","GIANT","GIDDY","GIPSY","GIRLY","GIRTH","GIVEN","GIVER","GLADE","GLAND","GLARE","GLASS","GLAZE","GLEAM","GLEAN","GLIDE","GLINT","GLOAT","GLOBE","GLOOM","GLORY","GLOSS","GLOVE","GLYPH","GNASH","GNOME","GODLY","GOING","GOLEM","GOLLY","GONAD","GONER","GOODY","GOOEY","GOOFY","GOOSE","GORGE","GOUGE","GOURD","GRACE","GRADE","GRAFT","GRAIL","GRAIN","GRAND","GRANT","GRAPE","GRAPH","GRASP","GRASS","GRATE","GRAVE","GRAVY","GRAZE","GREAT","GREED","GREEN","GREET","GRIEF","GRILL","GRIME","GRIMY","GRIND","GRIPE","GROAN","GROIN","GROOM","GROPE","GROSS","GROUP","GROUT","GROVE","GROWL","GROWN","GRUEL","GRUFF","GRUNT","GUARD","GUAVA","GUESS","GUEST","GUIDE","GUILD","GUILE","GUILT","GUISE","GULCH","GULLY","GUMBO","GUMMY","GUPPY","GUSTO","GUSTY","GYPSY","HABIT","HAIRY","HALVE","HANDY","HAPPY","HARDY","HAREM","HARPY","HARRY","HARSH","HASTE","HASTY","HATCH","HATER","HAUNT","HAUTE","HAVEN","HAVOC","HAZEL","HEADY","HEARD","HEART","HEATH","HEAVE","HEAVY","HEDGE","HEFTY","HEIST","HELIX","HELLO","HENCE","HERON","HILLY","HINGE","HIPPO","HIPPY","HITCH","HOARD","HOBBY","HOIST","HOLLY","HOMER","HONEY","HONOR","HORDE","HORNY","HORSE","HOTEL","HOTLY","HOUND","HOUSE","HOVEL","HOVER","HOWDY","HUMAN","HUMID","HUMOR","HUMPH","HUMUS","HUNCH","HUNKY","HURRY","HUSKY","HUSSY","HUTCH","HYDRO","HYENA","HYMEN","HYPER","ICILY","ICING","IDEAL","IDIOM","IDIOT","IDLER","IDYLL","IGLOO","ILIAC","IMAGE","IMBUE","IMPEL","IMPLY","INANE","INBOX","INCUR","INDEX","INEPT","INERT","INFER","INGOT","INLAY","INLET","INNER","INPUT","INTER","INTRO","IONIC","IRATE","IRONY","ISLET","ISSUE","ITCHY","IVORY","JAUNT","JAZZY","JELLY","JERKY","JETTY","JEWEL","JIFFY","JOINT","JOIST","JOKER","JOLLY","JOUST","JUDGE","JUICE","JUICY","JUMBO","JUMPY","JUNTA","JUNTO","JUROR","KAPPA","KARMA","KAYAK","KEBAB","KHAKI","KINKY","KIOSK","KITTY","KNACK","KNAVE","KNEAD","KNEED","KNEEL","KNELT","KNIFE","KNOCK","KNOLL","KNOWN","KOALA","KRILL","LABEL","LABOR","LADEN","LADLE","LAGER","LANCE","LANKY","LAPEL","LAPSE","LARGE","LARVA","LASSO","LATCH","LATER","LATHE","LATTE","LAUGH","LAYER","LEACH","LEAFY","LEAKY","LEANT","LEAPT","LEARN","LEASE","LEASH","LEAST","LEAVE","LEDGE","LEECH","LEERY","LEFTY","LEGAL","LEGGY","LEMON","LEMUR","LEPER","LEVEL","LEVER","LIBEL","LIEGE","LIGHT","LIKEN","LILAC","LIMBO","LIMIT","LINEN","LINER","LINGO","LIPID","LITHE","LIVER","LIVID","LLAMA","LOAMY","LOATH","LOBBY","LOCAL","LOCUS","LODGE","LOFTY","LOGIC","LOGIN","LOOPY","LOOSE","LORRY","LOSER","LOUSE","LOUSY","LOVER","LOWER","LOWLY","LOYAL","LUCID","LUCKY","LUMEN","LUMPY","LUNAR","LUNCH","LUNGE","LUPUS","LURCH","LURID","LUSTY","LYING","LYMPH","LYNCH","LYRIC","MACAW","MACHO","MACRO","MADAM","MADLY","MAFIA","MAGIC","MAGMA","MAIZE","MAJOR","MAKER","MAMBO","MAMMA","MAMMY","MANGA","MANGE","MANGO","MANGY","MANIA","MANIC","MANLY","MANOR","MAPLE","MARCH","MARRY","MARSH","MASON","MASSE","MATCH","MATEY","MAUVE","MAXIM","MAYBE","MAYOR","MEALY","MEANT","MEATY","MECCA","MEDAL","MEDIA","MEDIC","MELEE","MELON","MERCY","MERGE","MERIT","MERRY","METAL","METER","METRO","MICRO","MIDGE","MIDST","MIGHT","MILKY","MIMIC","MINCE","MINER","MINIM","MINOR","MINTY","MINUS","MIRTH","MISER","MISSY","MOCHA","MODAL","MODEL","MODEM","MOGUL","MOIST","MOLAR","MOLDY","MONEY","MONTH","MOODY","MOOSE","MORAL","MORON","MORPH","MOSSY","MOTEL","MOTIF","MOTOR","MOTTO","MOULT","MOUND","MOUNT","MOURN","MOUSE","MOUTH","MOVER","MOVIE","MOWER","MUCKY","MUCUS","MUDDY","MULCH","MUMMY","MUNCH","MURAL","MURKY","MUSHY","MUSIC","MUSKY","MUSTY","MYRRH","NADIR","NAIVE","NANNY","NASAL","NASTY","NATAL","NAVAL","NAVEL","NEEDY","NEIGH","NERDY","NERVE","NEVER","NEWER","NEWLY","NICER","NICHE","NIECE","NIGHT","NINJA","NINNY","NINTH","NOBLE","NOBLY","NOISE","NOISY","NOMAD","NOOSE","NORTH","NOSEY","NOTCH","NOVEL","NUDGE","NURSE","NUTTY","NYLON","NYMPH","OAKEN","OBESE","OCCUR","OCEAN","OCTAL","OCTET","ODDER","ODDLY","OFFAL","OFFER","OFTEN","OLDEN","OLDER","OLIVE","OMBRE","OMEGA","ONION","ONSET","OPERA","OPINE","OPIUM","OPTIC","ORBIT","ORDER","ORGAN","OTHER","OTTER","OUGHT","OUNCE","OUTDO","OUTER","OUTGO","OVARY","OVATE","OVERT","OVINE","OVOID","OWING","OWNER","OXIDE","OZONE","PADDY","PAGAN","PAINT","PALER","PALSY","PANEL","PANIC","PANSY","PAPAL","PAPER","PARER","PARKA","PARRY","PARSE","PARTY","PASTA","PASTE","PASTY","PATCH","PATIO","PATSY","PATTY","PAUSE","PAYEE","PAYER","PEACE","PEACH","PEARL","PECAN","PEDAL","PENAL","PENCE","PENNE","PENNY","PERCH","PERIL","PERKY","PESKY","PESTO","PETAL","PETTY","PHASE","PHONE","PHONY","PHOTO","PIANO","PICKY","PIECE","PIETY","PIGGY","PILOT","PINCH","PINEY","PINKY","PINTO","PIPER","PIQUE","PITCH","PITHY","PIVOT","PIXEL","PIXIE","PIZZA","PLACE","PLAID","PLAIN","PLAIT","PLANE","PLANK","PLANT","PLATE","PLAZA","PLEAD","PLEAT","PLIED","PLIER","PLUCK","PLUMB","PLUME","PLUMP","PLUNK","PLUSH","POESY","POINT","POISE","POKER","POLAR","POLKA","POLYP","POOCH","POPPY","PORCH","POSER","POSIT","POSSE","POUCH","POUND","POUTY","POWER","PRANK","PRAWN","PREEN","PRESS","PRICE","PRICK","PRIDE","PRIED","PRIME","PRIMO","PRINT","PRIOR","PRISM","PRIVY","PRIZE","PROBE","PRONE","PRONG","PROOF","PROSE","PROUD","PROVE","PROWL","PROXY","PRUDE","PRUNE","PSALM","PUBIC","PUDGY","PUFFY","PULPY","PULSE","PUNCH","PUPAL","PUPIL","PUPPY","PUREE","PURER","PURGE","PURSE","PUSHY","PUTTY","PYGMY","QUACK","QUAIL","QUAKE","QUALM","QUARK","QUART","QUASH","QUASI","QUEEN","QUEER","QUELL","QUERY","QUEST","QUEUE","QUICK","QUIET","QUILL","QUILT","QUIRK","QUITE","QUOTA","QUOTE","QUOTH","RABBI","RABID","RACER","RADAR","RADII","RADIO","RAINY","RAISE","RAJAH","RALLY","RALPH","RAMEN","RANCH","RANDY","RANGE","RAPID","RARER","RASPY","RATIO","RATTY","RAVEN","RAYON","RAZOR","REACH","REACT","READY","REALM","REARM","REBAR","REBEL","REBUS","REBUT","RECAP","RECUR","RECUT","REEDY","REFER","REFIT","REGAL","REHAB","REIGN","RELAX","RELAY","RELIC","REMIT","RENAL","RENEW","REPAY","REPEL","REPLY","RERUN","RESET","RESIN","RETCH","RETRO","RETRY","REUSE","REVEL","REVUE","RHINO","RHYME","RIDER","RIDGE","RIFLE","RIGHT","RIGID","RIGOR","RINSE","RIPEN","RIPER","RISEN","RISER","RISKY","RIVAL","RIVER","RIVET","ROACH","ROAST","ROBIN","ROBOT","ROCKY","RODEO","ROGER","ROGUE","ROOMY","ROOST","ROTOR","ROUGE","ROUGH","ROUND","ROUSE","ROUTE","ROVER","ROWDY","ROWER","ROYAL","RUDDY","RUDER","RUGBY","RULER","RUMBA","RUMOR","RUPEE","RURAL","RUSTY","SADLY","SAFER","SAINT","SALAD","SALLY","SALON","SALSA","SALTY","SALVE","SALVO","SANDY","SANER","SAPPY","SASSY","SATIN","SATYR","SAUCE","SAUCY","SAUNA","SAUTE","SAVOR","SAVOY","SAVVY","SCALD","SCALE","SCALP","SCALY","SCAMP","SCANT","SCARE","SCARF","SCARY","SCENE","SCENT","SCION","SCOFF","SCOLD","SCONE","SCOOP","SCOPE","SCORE","SCORN","SCOUR","SCOUT","SCOWL","SCRAM","SCRAP","SCREE","SCREW","SCRUB","SCRUM","SCUBA","SEDAN","SEEDY","SEGUE","SEIZE","SEMEN","SENSE","SEPIA","SERIF","SERUM","SERVE","SETUP","SEVEN","SEVER","SEWER","SHACK","SHADE","SHADY","SHAFT","SHAKE","SHAKY","SHALE","SHALL","SHALT","SHAME","SHANK","SHAPE","SHARD","SHARE","SHARK","SHARP","SHAVE","SHAWL","SHEAR","SHEEN","SHEEP","SHEER","SHEET","SHEIK","SHELF","SHELL","SHIED","SHIFT","SHINE","SHINY","SHIRE","SHIRK","SHIRT","SHOAL","SHOCK","SHONE","SHOOK","SHOOT","SHORE","SHORN","SHORT","SHOUT","SHOVE","SHOWN","SHOWY","SHREW","SHRUB","SHRUG","SHUCK","SHUNT","SHUSH","SHYLY","SIEGE","SIEVE","SIGHT","SIGMA","SILKY","SILLY","SINCE","SINEW","SINGE","SIREN","SISSY","SIXTH","SIXTY","SKATE","SKIER","SKIFF","SKILL","SKIMP","SKIRT","SKULK","SKULL","SKUNK","SLACK","SLAIN","SLANG","SLANT","SLASH","SLATE","SLAVE","SLEEK","SLEEP","SLEET","SLEPT","SLICE","SLICK","SLIDE","SLIME","SLIMY","SLING","SLINK","SLOOP","SLOPE","SLOSH","SLOTH","SLUMP","SLUNG","SLUNK","SLURP","SLUSH","SLYLY","SMACK","SMALL","SMART","SMASH","SMEAR","SMELL","SMELT","SMILE","SMIRK","SMITE","SMITH","SMOCK","SMOKE","SMOKY","SMOTE","SNACK","SNAIL","SNAKE","SNAKY","SNARE","SNARL","SNEAK","SNEER","SNIDE","SNIFF","SNIPE","SNOOP","SNORE","SNORT","SNOUT","SNOWY","SNUCK","SNUFF","SOAPY","SOBER","SOGGY","SOLAR","SOLID","SOLVE","SONAR","SONIC","SOOTH","SOOTY","SORRY","SOUND","SOUTH","SOWER","SPACE","SPADE","SPANK","SPARE","SPARK","SPASM","SPAWN","SPEAK","SPEAR","SPECK","SPEED","SPELL","SPELT","SPEND","SPENT","SPERM","SPICE","SPICY","SPIED","SPIEL","SPIKE","SPIKY","SPILL","SPILT","SPINE","SPINY","SPIRE","SPITE","SPLAT","SPLIT","SPOIL","SPOKE","SPOOF","SPOOK","SPOOL","SPOON","SPORE","SPORT","SPOUT","SPRAY","SPREE","SPRIG","SPUNK","SPURN","SPURT","SQUAD","SQUAT","SQUIB","STACK","STAFF","STAGE","STAID","STAIN","STAIR","STAKE","STALE","STALK","STALL","STAMP","STAND","STANK","STARE","STARK","START","STASH","STATE","STAVE","STEAD","STEAK","STEAL","STEAM","STEED","STEEL","STEEP","STEER","STEIN","STERN","STICK","STIFF","STILL","STILT","STING","STINK","STINT","STOCK","STOIC","STOKE","STOLE","STOMP","STONE","STONY","STOOD","STOOL","STOOP","STORE","STORK","STORM","STORY","STOUT","STOVE","STRAP","STRAW","STRAY","STRIP","STRUT","STUCK","STUDY","STUFF","STUMP","STUNG","STUNK","STUNT","STYLE","SUAVE","SUGAR","SUING","SUITE","SULKY","SULLY","SUMAC","SUNNY","SUPER","SURER","SURGE","SURLY","SUSHI","SWAMI","SWAMP","SWARM","SWASH","SWATH","SWEAR","SWEAT","SWEEP","SWEET","SWELL","SWEPT","SWIFT","SWILL","SWINE","SWING","SWIRL","SWISH","SWOON","SWOOP","SWORD","SWORE","SWORN","SWUNG","SYNOD","SYRUP","TABBY","TABLE","TABOO","TACIT","TACKY","TAFFY","TAINT","TAKEN","TAKER","TALLY","TALON","TAMER","TANGO","TANGY","TAPER","TAPIR","TARDY","TAROT","TASTE","TASTY","TATTY","TAUNT","TAWNY","TEACH","TEARY","TEASE","TEDDY","TEETH","TEMPO","TENET","TENOR","TENSE","TENTH","TEPEE","TEPID","TERRA","TERSE","TESTY","THANK","THEFT","THEIR","THEME","THERE","THESE","THETA","THICK","THIEF","THIGH","THING","THINK","THIRD","THONG","THORN","THOSE","THREE","THREW","THROB","THROW","THRUM","THUMB","THUMP","THYME","TIARA","TIBIA","TIDAL","TIGER","TIGHT","TILDE","TIMER","TIMID","TIPSY","TITAN","TITHE","TITLE","TOAST","TODAY","TODDY","TOKEN","TONAL","TONGA","TONIC","TOOTH","TOPAZ","TOPIC","TORCH","TORSO","TORUS","TOTAL","TOTEM","TOUCH","TOUGH","TOWEL","TOWER","TOXIC","TOXIN","TRACE","TRACK","TRACT","TRADE","TRAIL","TRAIN","TRAIT","TRAMP","TRASH","TRAWL","TREAD","TREAT","TREND","TRIAD","TRIAL","TRIBE","TRICE","TRICK","TRIED","TRIPE","TRITE","TROLL","TROOP","TROPE","TROUT","TROVE","TRUCE","TRUCK","TRUER","TRULY","TRUMP","TRUNK","TRUSS","TRUST","TRUTH","TRYST","TUBAL","TUBER","TULIP","TULLE","TUMOR","TUNIC","TURBO","TUTOR","TWANG","TWEAK","TWEED","TWEET","TWICE","TWINE","TWIRL","TWIST","TWIXT","TYING","UDDER","ULCER","ULTRA","UMBRA","UNCLE","UNCUT","UNDER","UNDID","UNDUE","UNFED","UNFIT","UNIFY","UNION","UNITE","UNITY","UNLIT","UNMET","UNSET","UNTIE","UNTIL","UNWED","UNZIP","UPPER","UPSET","URBAN","URINE","USAGE","USHER","USING","USUAL","USURP","UTILE","UTTER","VAGUE","VALET","VALID","VALOR","VALUE","VALVE","VAPID","VAPOR","VAULT","VAUNT","VEGAN","VENOM","VENUE","VERGE","VERSE","VERSO","VERVE","VICAR","VIDEO","VIGIL","VIGOR","VILLA","VINYL","VIOLA","VIPER","VIRAL","VIRUS","VISIT","VISOR","VISTA","VITAL","VIVID","VIXEN","VOCAL","VODKA","VOGUE","VOICE","VOILA","VOMIT","VOTER","VOUCH","VOWEL","VYING","WACKY","WAFER","WAGER","WAGON","WAIST","WAIVE","WALTZ","WARTY","WASTE","WATCH","WATER","WAVER","WAXEN","WEARY","WEAVE","WEDGE","WEEDY","WEIGH","WEIRD","WELCH","WELSH","WENCH","WHACK","WHALE","WHARF","WHEAT","WHEEL","WHELP","WHERE","WHICH","WHIFF","WHILE","WHINE","WHINY","WHIRL","WHISK","WHITE","WHOLE","WHOOP","WHOSE","WIDEN","WIDER","WIDOW","WIDTH","WIELD","WIGHT","WILLY","WIMPY","WINCE","WINCH","WINDY","WISER","WISPY","WITCH","WITTY","WOKEN","WOMAN","WOMEN","WOODY","WOOER","WOOLY","WOOZY","WORDY","WORLD","WORRY","WORSE","WORST","WORTH","WOULD","WOUND","WOVEN","WRACK","WRATH","WREAK","WRECK","WREST","WRING","WRIST","WRITE","WRONG","WROTE","WRUNG","WRYLY","YACHT","YEARN","YEAST","YIELD","YOUNG","YOUTH","ZEBRA","ZESTY","ZONAL"};
int NUM_WORDS = sizeof(WORDLIST) / sizeof(WORDLIST[0]);

typedef const char* Word;
typedef vector<Word> Words;

#define BIT(var,pos) ((var) & (1<<(pos)))
#define FOR5(i) for (int i = 0; i < 5; i++)

#define UNSET_CHAR ('Z' + 1)

struct Needed {
	char c;
	int minCount;
	int maxCount;
};

struct Constraint {
	// Partial constraint (used for caching possible values)
	uint32_t banned; // Grey letters in the form of a bitmask, NOTE: Only looks at a few letters here to reduce search space
	Needed needed[5]; // Yellow letters (including information on number of letters)
	char exact[5]; // Green letters

	// Full constraint (too big a search space to use for caching)
	int possibilities[5];

	bool operator==(const Constraint &other) const {
		return memcmp(this, &other, sizeof(Constraint)) == 0;
	}
};

Constraint baseConstraint() {
	Constraint c;
	memset(&c, 0, sizeof(c));
	FOR5(i) c.possibilities[i] = -1;
	FOR5(i) c.exact[i] = UNSET_CHAR;
	FOR5(i) c.needed[i] = Needed{UNSET_CHAR,0,0};
	return c;
}

int countInWord(Word w, char c) {
	int count = 0;
	FOR5(i) {
		if (w[i] == c) {
			count++;
		}
	}
	return count;
}

unordered_map<__int128, Words> partialToGuesses;
const Words& guessesWithPartialConstraint(const Constraint& c) {
	__int128 key = 0;
	int ind = 0;

	auto addToKey = [&](int val, int len) {
		key += (__int128)val << ind;
		ind += len;
	};

	addToKey(c.banned, 26);
	addToKey(c.needed[0].c & 0b11111, 5);
	addToKey(c.needed[1].c & 0b11111, 5);
	addToKey(c.needed[2].c & 0b11111, 5);
	addToKey(c.needed[3].c & 0b11111, 5);
	addToKey(c.needed[4].c & 0b11111, 5);
	addToKey(c.needed[0].minCount & 0b11, 2);
	addToKey(c.needed[1].minCount & 0b11, 2);
	addToKey(c.needed[2].minCount & 0b11, 2);
	addToKey(c.needed[3].minCount & 0b11, 2);
	addToKey(c.needed[4].minCount & 0b11, 2);
	addToKey(c.needed[0].maxCount & 0b11, 2);
	addToKey(c.needed[1].maxCount & 0b11, 2);
	addToKey(c.needed[2].maxCount & 0b11, 2);
	addToKey(c.needed[3].maxCount & 0b11, 2);
	addToKey(c.needed[4].maxCount & 0b11, 2);
	addToKey(c.exact[0] & 0b11111, 5);
	addToKey(c.exact[1] & 0b11111, 5);
	addToKey(c.exact[2] & 0b11111, 5);
	addToKey(c.exact[3] & 0b11111, 5);
	addToKey(c.exact[4] & 0b11111, 5);

	auto it = partialToGuesses.find(key);
	if (it != partialToGuesses.end()) {
		return it->second;
	}

	Words words;
	for (Word w : WORDLIST) {
		int contains = 0;
		FOR5(i) {
			contains |= 1 << (w[i] - 'A');
		}
		if (contains & c.banned) {
			continue;
		}
		bool valid = true;
		FOR5(i) {
			if (c.needed[i].c != UNSET_CHAR) {
				int count = countInWord(w, c.needed[i].c);
				if (count < c.needed[i].minCount || count > c.needed[i].maxCount) {
					valid = false;
					break;
				}
			}
		}
		if (!valid) continue;

		FOR5(i) if (c.exact[i] != UNSET_CHAR && c.exact[i] != w[i]) valid = false;
		if (!valid) continue;

		words.push_back(w);
	}
	partialToGuesses[key] = words;
	return partialToGuesses[key];
}

Words validsForDepth[32];
const Words& validGuesses(const Constraint& c, int depth) {
	Words& words = validsForDepth[depth];
	words.clear();
	for (Word w : guessesWithPartialConstraint(c)) {
		bool valid = true;
		FOR5(i) {
			if (!BIT(c.possibilities[i], w[i] - 'A')) {
				valid = false;
			}
		}
		if (valid) {
			words.push_back(w);
		}
	}
	return words;
}

// Keeping track of every banned letter increases search space too much so just look at a couple
int COMMON = (1 << ('E' - 'A')) | (1 << ('R' - 'A')) | (1 << ('A' - 'A')) | (1 << ('O' - 'A')) | (1 << ('T' - 'A')) | (1 << ('S' - 'A'));

enum Color { WHITE, GREEN, YELLOW, GREY };

unordered_map<Word, Constraint> guessesToConstraint; // DEPENDS ON ANSWER
Constraint narrowConstraint(const Constraint& c, Word guess, Word answer) {
	Constraint newC;
	if (guessesToConstraint.count(guess) > 0) {
		newC = guessesToConstraint[guess];
	} else {
		newC = baseConstraint();
		bool accountedForInAnswer[5] = {false};
		Color guessColors[5] = {WHITE};

		// First allocate any green letters
		FOR5(i) {
			if (answer[i] == guess[i]) {
				int add = 1 << (guess[i] - 'A'); // Mask to add this letter
				newC.possibilities[i] = add;
				newC.exact[i] = guess[i];
				accountedForInAnswer[i] = true;
				guessColors[i] = GREEN;
			}
		}

		// Then from left to right set yellow if not accounted for
		FOR5(i) {
			if (answer[i] != guess[i]) {
				int add = 1 << (guess[i] - 'A'); // Mask to add this letter
				int remove = ~add; // Mask to remove this letter

				bool shouldBan = true;
				int ind = -1;
				FOR5(j) {
					if (i != j && guess[i] == answer[j]) {
						shouldBan = false;
						if (!accountedForInAnswer[j]) {
							accountedForInAnswer[j] = true;
							ind = j;
							break;
						}
					}
				}
				if (shouldBan) {
					// If doesn't exist anywhere in the word then ban it
					newC.banned |= add;
					FOR5(j) newC.possibilities[j] &= remove;
				}
				if (ind == -1) {
					// Either doesn't exist or already accounted for earlier
					newC.possibilities[i] &= remove;
					guessColors[i] = GREY;
				} else {
					// Should mark as yellow if we are the first letter to account for a certain letter in the answer
					newC.possibilities[i] &= remove;
					guessColors[i] = YELLOW;
				}
			}
		}

		// If any letters were accounted for (with a green or yellow) in some part of the word but grey elsewhere, then we know how many must exist
		for (char c = 'A'; c <= 'Z'; c++) {
			int guessedC = 0;
			int greenOrYellowC = 0;
			FOR5(i) {
				if (guess[i] == c) {
					guessedC += 1;
					if (guessColors[i] == GREEN || guessColors[i] == YELLOW) {
						greenOrYellowC += 1;
					}
				}
			}

			if (greenOrYellowC > 0) {
				Needed n;
				n.c = c;
				n.minCount = greenOrYellowC;
				if (guessedC == greenOrYellowC) {
					n.maxCount = 3; // No Words with more than 3 of same letter
				} else {
					n.maxCount = greenOrYellowC;
				}

				// Place needed char at end of needed list
				FOR5(i) {
					if (newC.needed[i].c == UNSET_CHAR) {
						newC.needed[i] = n;
						break;
					}
				}
			}
		}
	}

	// Combine
	newC.banned |= c.banned;
	newC.banned &= COMMON;
	FOR5(i) {
		newC.possibilities[i] &= c.possibilities[i];
		if (c.exact[i] != UNSET_CHAR) {
			newC.exact[i] = c.exact[i];
		}

		// Merge needed lists together
		if (c.needed[i].c != UNSET_CHAR) {
			FOR5(j) {
				if (newC.needed[j].c == c.needed[i].c) {
					newC.needed[j].minCount = max(newC.needed[j].minCount, c.needed[i].minCount);
					newC.needed[j].maxCount = min(newC.needed[j].maxCount, c.needed[i].maxCount);
					break;
				} else if (newC.needed[j].c == UNSET_CHAR) {
					newC.needed[j] = c.needed[i];
					break;
				}
			}
		}
	}

	// Sort needed list so keys match up
	sort(newC.needed, newC.needed + 5, [](const Needed& n1, const Needed& n2){
		return n1.c < n2.c;
	});

	bool cHasCorrectR = false;
	FOR5(i) {
		if (c.needed[i].c == 'R' && c.needed[i].minCount == 1 && c.needed[i].maxCount == 1) cHasCorrectR = true;
	}
	bool newcHasCorrectR = false;
	FOR5(i) {
		if (newC.needed[i].c == 'R' && newC.needed[i].minCount == 1 && newC.needed[i].maxCount == 1) newcHasCorrectR = true;
	}
	if (cHasCorrectR && ! newcHasCorrectR) {
		exit(1);
	}

	return newC;
}

vector<bool> triedFirstTwo(NUM_WORDS * NUM_WORDS, false); // NEEDS TO BE CLEARED WHEN ANSWER CHANGES
Word curFirst;
bool alreadyTried(Word first, Word second) {
	if (first > second) {
		swap(first, second);
	}
	int index = (first - WORDLIST[0]) / 6 * NUM_WORDS + (second - WORDLIST[0]) / 6;
	if (index >= NUM_WORDS * NUM_WORDS) {
		exit(1);
	}

	if (triedFirstTwo[index]) {
		return true;
	}
	triedFirstTwo[index] = true;
	return false;
}

Words curLongestSequenceForDepth[32];
int longestSeenForCurWord = 0;
const Words& getLongestSequence(int depth, Constraint c, Word answer) {
	Words& maxSequence = curLongestSequenceForDepth[depth];
	maxSequence.clear();

	const Words& guesses = validGuesses(c, depth);
	if (depth + guesses.size() <= longestSeenForCurWord) {
		return maxSequence;
	}

	for (Word w : guesses) {
		if (w != answer) {
			if (depth == 0) curFirst = w;
			if (depth == 1) {
				// Can prune up to half the search tree by skipping any pair of the first two words that have already been tried in a different order
				if (alreadyTried(curFirst, w)) {
					continue;
				}
			}

			Constraint nextC = narrowConstraint(c, w, answer);
			const Words& seq = getLongestSequence(depth + 1, nextC, answer);

			if (depth == 0) {
				cout << "\33[2K\rWorking on " << answer << " (" << w << ")" << flush;
			}

			if (seq.size() + 1 > maxSequence.size()) {
				maxSequence = seq;
				maxSequence.push_back(w);
			}
		}
	}

	if (maxSequence.size() == 0) {
		maxSequence.push_back(answer);
	}

	longestSeenForCurWord = max(longestSeenForCurWord, depth + (int)maxSequence.size());

	return maxSequence;
}

int main(int argc, char* argv[]) {
	int start = 0;
	int end = NUM_WORDS;

	if (argc > 1) {
		start = atoi(argv[1]);
	}
	if (argc > 2) {
		end = atoi(argv[2]);
	}
	cout << "Working from " << WORDLIST[start] << " to " << WORDLIST[end - 1] << endl;

	for (int i = start; i < end; i++) {
		Word answer = WORDLIST[i];
		auto start = chrono::high_resolution_clock::now();

		Words seq = getLongestSequence(0, baseConstraint(), answer);
		cout << "\33[2K\r";
		for (auto it = seq.rbegin(); it != seq.rend(); it++) cout << *it << " ";
		cout << "(" << seq.size() << ") in " << chrono::duration_cast<chrono::seconds>(chrono::high_resolution_clock::now() - start).count() << " seconds" << endl;
		guessesToConstraint.clear();
		triedFirstTwo = vector<bool>(NUM_WORDS * NUM_WORDS, false);
		longestSeenForCurWord = 0;
	}
}
