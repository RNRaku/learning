#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<time.h>

int main(){
    int i,j,k;
    int p=0,found=0;

    char list[][6]={"aback","assay","blurb","chafe","craze","draft","faint","front","guess","itchy","lucky","music","perch","queer","rupee","shrug","sperm","swear","track","vital","abase","asset","blurt","chaff","crazy","drain","fairy","frost","guest","ivory","lumen","musky","peril","quell","rural","shuck","spice","sweat","tract","vivid","abate","atoll","blush","chain","creak","drake","faith","froth","guide","jaunt","lumpy","musty","perky","query","rusty","shunt","spicy","sweep","trade","vixen","abbey","atone","board","chair","cream","drama","fancy","frown","guild","jazzy","lunar","myrrh","pesky","quest","sadly","shush","spied","sweet","trail","vocal","abbot","attic","boast","chalk","credo","drank","fanny","froze","guile","jelly","lunch","nadir","pesto","queue","safer","shyly","spiel","swell","train","vodka","abhor","audio","bobby","champ","creed","drape","farce","fruit","guilt","jerky","lunge","naive","petal","quick","saint","siege","spike","swept","trait","vogue","abide","audit","boney","chant","creek","drawl","fatal","fudge","guise","jetty","lupus","nanny","petty","quiet","salad","sieve","spiky","swift","tramp","voice","abled","augur","bongo","chaos","creep","drawn","fatty","fugue","gulch","jewel","lurch","nasal","phase","quill","sally","sight","spill","swill","trash","voila","abode","aunty","bonus","chard","creme","dread","fault","fully","gully","jiffy","lurid","nasty","phone","quilt","salon","sigma","spilt","swine","trawl","vomit","abort","avail","booby","charm","crepe","dream","fauna","fungi","gumbo","joint","lusty","natal","phony","quirk","salsa","silky","spine","swing","tread","voter","about","avert","boost","chart","crept","dress","favor","funky","gummy","joist","lying","naval","photo","quite","salty","silly","spiny","swirl","treat","vouch","above","avian","booth","chase","cress","dried","feast","funny","guppy","joker","lymph","navel","piano","quota","salve","since","spire","swish","trend","vowel","abuse","avoid","booty","chasm","crest","drier","fecal","furor","gusto","jolly","lyric","needy","picky","quote","salvo","sinew","spite","swoon","triad","vying","abyss","await","booze","cheap","crick","drift","feign","furry","gusty","joust","macaw","neigh","piece","quoth","sandy","singe","splat","swoop","trial","wacky","acorn","awake","boozy","cheat","cried","drill","fella","fussy","gypsy","judge","macho","nerdy","piety","rabbi","saner","siren","split","sword","tribe","wafer","acrid","award","borax","check","crier","drink","felon","fuzzy","habit","juice","macro","nerve","piggy","rabid","sappy","sissy","spoil","swore","trice","wager","actor","aware","borne","cheek","crime","drive","femme","gaffe","hairy","juicy","madam","never","pilot","racer","sassy","sixth","spoke","sworn","trick","wagon","acute","awash","bosom","cheer","crimp","droit","femur","gaily","halve","jumbo","madly","newer","pinch","radar","satin","sixty","spoof","swung","tried","waist","adage","awful","bossy","chess","crisp","droll","fence","gamer","handy","jumpy","mafia","newly","piney","radii","satyr","skate","spook","synod","tripe","waive","adapt","awoke","botch","chest","croak","drone","feral","gamma","happy","junta","magic","nicer","pinky","radio","sauce","skier","spool","syrup","trite","waltz","adept","axial","bough","chick","crock","drool","ferry","gamut","hardy","junto","magma","niche","pinto","rainy","saucy","skiff","spoon","tabby","troll","warty","adieu","axiom","boule","chide","crone","droop","fetal","gassy","harem","juror","maize","niece","piper","raise","sauna","skill","spore","table","troop","waste","admin","axion","bound","chief","crony","dross","fetch","gaudy","harpy","kappa","major","night","pique","rajah","saute","skimp","sport","taboo","trope","watch","admit","azure","bowel","child","crook","drove","fetid","gauge","harry","karma","maker","ninja","pitch","rally","savor","skirt","spout","tacit","trout","water","adobe","bacon","boxer","chili","cross","drown","fetus","gaunt","harsh","kayak","mambo","ninny","pithy","ralph","savoy","skulk","spray","tacky","trove","waver","adopt","badge","brace","chill","croup","druid","fever","gauze","haste","kebab","mamma","ninth","pivot","ramen","savvy","skull","spree","taffy","truce","waxen","adore","badly","braid","chime","crowd","drunk","fewer","gavel","hasty","khaki","mammy","noble","pixel","ranch","scald","skunk","sprig","taint","truck","weary","adorn","bagel","brain","china","crown","dryer","fiber","gawky","hatch","kinky","manga","nobly","pixie","randy","scale","slack","spunk","taken","truer","weave","adult","baggy","brake","chirp","crude","dryly","ficus","gayer","hater","kiosk","mange","noise","pizza","range","scalp","slain","spurn","taker","truly","wedge","affix","baker","brand","chock","cruel","duchy","field","gayly","haunt","kitty","mango","noisy","place","rapid","scaly","slang","spurt","tally","trump","weedy","afire","baler","brash","choir","crumb","dully","fiend","gazer","haute","knack","mangy","nomad","plaid","rarer","scamp","slant","squad","talon","trunk","weigh","afoot","balmy","brass","choke","crump","dummy","fiery","gecko","haven","knave","mania","noose","plain","raspy","scant","slash","squat","tamer","truss","weird","afoul","banal","brave","chord","crush","dumpy","fifth","geeky","havoc","knead","manic","north","plait","ratio","scare","slate","squib","tango","trust","welch","after","banjo","bravo","chore","crust","dunce","fifty","geese","hazel","kneed","manly","nosey","plane","ratty","scarf","sleek","stack","tangy","truth","welsh","again","barge","brawl","chose","crypt","dusky","fight","genie","heady","kneel","manor","notch","plank","raven","scary","sleep","staff","taper","tryst","whack","agape","baron","brawn","chuck","cubic","dusty","filer","genre","heard","knelt","maple","novel","plant","rayon","scene","sleet","stage","tapir","tubal","whale","agate","basal","bread","chump","cumin","dutch","filet","ghost","heart","knife","march","nudge","plate","razor","scent","slept","staid","tardy","tuber","wharf","agent","basic","break","chunk","curio","duvet","filly","ghoul","heath","knock","marry","nurse","plaza","reach","scion","slice","stain","tarot","tulip","wheat","agile","basil","breed","churn","curly","dwarf","filmy","giant","heave","knoll","marsh","nutty","plead","react","scoff","slick","stair","taste","tulle","wheel","aging","basin","briar","chute","curry","dwell","filth","giddy","heavy","known","mason","nylon","pleat","ready","scold","slide","stake","tasty","tumor","whelp","aglow","basis","bribe","cider","curse","dwelt","final","gipsy","hedge","koala","masse","nymph","plied","realm","scone","slime","stale","tatty","tunic","where","agony","baste","brick","cigar","curve","dying","finch","girly","hefty","krill","match","oaken","plier","rearm","scoop","slimy","stalk","taunt","turbo","which","agree","batch","bride","cinch","curvy","eager","finer","girth","heist","label","matey","obese","pluck","rebar","scope","sling","stall","tawny","tutor","whiff","ahead","bathe","brief","circa","cutie","eagle","first","given","helix","labor","mauve","occur","plumb","rebel","score","slink","stamp","teach","twang","while","aider","baton","brine","civic","cyber","early","fishy","giver","hello","laden","maxim","ocean","plume","rebus","scorn","sloop","stand","teary","tweak","whine","aisle","batty","bring","civil","cycle","earth","fixer","glade","hence","ladle","maybe","octal","plump","rebut","scour","slope","stank","tease","tweed","whiny","alarm","bawdy","brink","clack","cynic","easel","fizzy","gland","heron","lager","mayor","octet","plunk","recap","scout","slosh","stare","teddy","tweet","whirl","album","bayou","briny","claim","daddy","eaten","fjord","glare","hilly","lance","mealy","odder","plush","recur","scowl","sloth","stark","teeth","twice","whisk","alert","beach","brisk","clamp","daily","eater","flack","glass","hinge","lanky","meant","oddly","poesy","recut","scram","slump","start","tempo","twine","white","algae","beady","broad","clang","dairy","ebony","flail","glaze","hippo","lapel","meaty","offal","point","reedy","scrap","slung","stash","tenet","twirl","whole","lucid","beard","broil","clank","daisy","eclat","flair","gleam","hippy","lapse","mecca","offer","poise","refer","scree","slunk","state","tenor","twist","whoop","alien","beast","broke","clash","dally","edict","flake","glean","hitch","large","medal","often","poker","refit","screw","slurp","stave","tense","twixt","whose","align","beech","brood","clasp","dance","edify","flaky","glide","hoard","larva","media","olden","polar","regal","scrub","slush","stead","tenth","tying","widen","alike","beefy","brook","class","dandy","eerie","flame","glint","hobby","lasso","medic","older","polka","rehab","scrum","slyly","steak","tepee","udder","wider","alive","befit","broom","clean","datum","egret","flank","gloat","hoist","latch","melee","olive","polyp","reign","scuba","smack","steal","tepid","ulcer","widow","allay","began","broth","clear","daunt","eight","flare","globe","holly","later","melon","ombre","pooch","relax","sedan","small","steam","terra","ultra","width","alley","begat","brown","cleat","dealt","eject","flash","gloom","homer","lathe","mends","omega","poppy","relay","seedy","smart","steed","terse","umbra","wield","allot","beget","brunt","cleft","death","eking","flask","glory","honey","latte","mercy","onion","porch","relic","segue","smash","steel","testy","uncle","wight","allow","begin","brush","clerk","debar","elate","fleck","gloss","honor","laugh","merge","onset","poser","remit","seize","smear","steep","thank","uncut","willy","alloy","begun","brute","click","debit","elbow","fleet","glove","horde","layer","merit","opera","posit","renal","semen","smell","steer","theft","under","wimpy","aloft","being","buddy","cliff","debug","elder","flesh","glyph","horny","leach","merry","opine","posse","renew","sense","smelt","stein","their","undid","wince","alone","belch","budge","climb","debut","elect","flick","gnash","horse","leafy","metal","opium","pouch","repay","sepia","smile","stern","theme","undue","winch","along","belie","buggy","cling","decal","elegy","flier","gnome","hotel","leaky","meter","optic","pound","repel","serif","smirk","stick","there","unfed","windy","aloof","belle","bugle","clink","decay","elfin","fling","godly","hotly","leant","metro","orbit","pouty","reply","serum","smite","stiff","these","unfit","wiser","aloud","belly","build","cloak","decor","elide","flint","going","hound","leapt","micro","order","power","rerun","serve","smith","still","theta","unify","wispy","alpha","below","built","clock","decoy","elite","flirt","golem","house","learn","midge","organ","prank","reset","setup","smock","stilt","thick","union","witch","altar","bench","bulge","clone","decry","elope","float","golly","hovel","lease","midst","other","prawn","resin","seven","smoke","sting","thief","unite","witty","alter","beret","bulky","close","defer","elude","flock","gonad","hover","leash","might","otter","preen","retch","sever","smoky","stink","mound","patch","puree","rotor","shirk","sower","suing","tonic","verse","youth","arbor","blimp","carat","cover","dodge","evict","frail","groan","inept","lodge","mount","patio","purer","rouge","shirt","space","suite","tooth","verso","zebra","ardor","blind","cargo","covet","dodgy","evoke","frame","groin","inert","lofty","mourn","patsy","purge","rough","shoal","spade","sulky","topaz","verve","zesty","arena","blink","carol","covey","dogma","exact","frank","groom","infer","logic","mouse","patty","purse","round","shock","spank","sully","topic","vicar","zonal","argue","bliss","carry","cower","doing","exalt","fraud","grope","ingot","login","mouth","pause","pushy","rouse","shone","spare","sumac","torch","video","askew","arise","blitz","carve","coyly","dolly","excel","freak","gross","inlay","loopy","mover","payee","putty","route","shook","spark","sunny","torso","vigil","mushy","armor","bloat","caste","crack","donor","exert","freed","group","inlet","loose","movie","payer","pygmy","rover","shoot","spasm","super","torus","vigor","vista","aroma","block","catch","craft","donut","exile","freer","grout","inner","lorry","mower","peace","quack","rowdy","shore","spawn","surer","total","villa","trace","arose","bloke","cater","cramp","dopey","exist","fresh","grove","input","loser","mucky","peach","quail","rower","shorn","speak","surge","totem","vinyl","swath","array","blond","catty","crane","doubt","expel","friar","growl","inter","louse","mucus","pearl","quake","royal","short","spear","surly","touch","viola","spent","arrow","blood","caulk","crank","dough","extol","fried","grown","intro","lousy","muddy","pecan","qualm","ruddy","shout","speck","sushi","tough","viper","shrub","arson","bloom","cause","crash","dowdy","extra","frill","gruel","ionic","lover","mulch","pedal","quark","ruder","shove","speed","swami","towel","viral","rumor","artsy","blown","cavil","crass","dowel","exult","frisk","gruff","irate","lower","mummy","penal","quart","rugby","shown","spell","swamp","tower","virus","queen","ascot","bluer","cease","crate","downy","eying","fritz","grunt","irony","lowly","munch","pence","quash","ruler","showy","spelt","swarm","toxic","visit","penny","ashen","bluff","cedar","crave","dowry","fable","frock","guard","islet","loyal","mural","penne","quasi","rumba","shrew","spend","swash","toxin","visor","murky","aside","blunt","cello","crawl","dozen","facet","frond","guava","issue","lucid"};
    
    char guess[6];
    char word[6];

while(p!=1){

    srand(time(NULL));
    int n = rand()%2031;
    int m=5;
    //printf("%d\n",n);
    strcpy(word, list[n]);

    printf("*****************************************************************************************************************************\n ");
    printf("\n");

    printf("Welcome to WORDEL\n");
    printf("Your objective is to correctly guess the 5 letter word i am thinking about right now\n");
    printf("you can only enter valid 5 letter english words");
    printf("\n");
    printf("If you guess the correct alphabet at the correct positon ill give you a GREEN \n");
    printf("If you guess the correct alphabet but at the incorrect positon ill give you a ORANGE \n");
    printf("If you guess the incorrect alphabet ill give you a GREY\n");


    while(m!=0){
        printf("\n");
        printf("You have %d guesses left \n",m);
        printf("Enter your guess  :");
        scanf("%s",&guess);

        /*found = 0;
        for (i = 0; i < 2031; i++) {
            if (strcmp(guess, list[i]) == 0) {
                found = 1;
                break;
            }
        }
        if (!found) {
            printf("Invalid word.... You loseeeeeee\n");
            printf("Try entering a real 5-letter English word next time.\n");
            printf("*****************************************************************************************************************************\n ");
            break;
        }

        for (i = 0; i < 2031; i++) {
            found=0;
            for (j = 0; j < 6; j++) {
                if(guess[j]==list[j][i]){
                    found=found+1;
                    printf("%d\n",found);
                    if (found==5){
                        break;
                    }
                }               
            }
        }
        if (found!=5){
            printf("invalid word.... You loseeeeeee \n");
            printf("try entering a real 5 letter english word next time\n");
             printf("*****************************************************************************************************************************\n ");
            break;
        }*/


        printf("\n");
        k=0;
        
            for (i = 0; i < strlen(word); i++) {
                if (word[i]==guess[i]){
                    printf("🟩GREEN \t");
                    k=k+1;
                }
                else if (guess[i]!=word[i]){
                    for (j = 0; j < strlen(word); j++) {
                        if (guess[i]==word[j]){
                            printf("🟧OGANGE \t");
                            break;
                        }
                    }
                }
                if (j == strlen(word)){
                    printf("⬛GREY \t");
                }
            }
            printf("\n");
            m=m-1;
        if(k==5){
            printf("WORDEL!!!!!!!!!");
            printf("you guesses correctly, you win!");
            break;
        }
    }
    if(m==0){
        printf("Bad luck, you lose \n");
        printf("The correct word was %s \n",word);
    }
    printf("Would you like to play again? \n");
    printf("Enter 0 to play again\nEnter 1 to exit \n");
    scanf("%d",&p);
}
}