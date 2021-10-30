#include "bignum.hpp"
#include "scheme_fixture.hpp"

using namespace insider;

struct numeric : scheme_fixture { };

TEST_F(numeric, bignum_add_subtract) {
  auto make_small = [&] (integer::value_type v) {
    return integer_to_ptr(integer{v});
  };

#define TEST_ADD1(lhs, rhs, result) EXPECT_TRUE(num_equal(add(ctx, lhs, rhs), result))

  TEST_ADD1(make_big_literal(ctx, {limb_max}), make_big_literal(ctx, {1}), make_big_literal(ctx, {0, 1}));
  TEST_ADD1(make_big_literal(ctx, {limb_max}), make_big_literal(ctx, {5}), make_big_literal(ctx, {4, 1}));
  TEST_ADD1(make_big(ctx, 3, 2, 1), make_big(ctx, 6, 5, 4), make_big(ctx, 9, 7, 5));
  TEST_ADD1(make_big(ctx, 0), make_big(ctx, 17), make_big(ctx, 17));
  TEST_ADD1(make_big_literal(ctx, {limb_max, limb_max, limb_max}), make_small(1), make_big_literal(ctx, {0, 0, 0, 1}));
  TEST_ADD1(make_big_literal(ctx, {limb_max - 1}), make_small(1), make_big_literal(ctx, {limb_max}));
  TEST_ADD1(make_big_literal(ctx, {limb_max - 1}), make_small(2), make_big_literal(ctx, {0, 1}));
  TEST_ADD1(make_big_literal(ctx, {limb_max, 1}), make_small(1), make_big_literal(ctx, {0, 2}));
  TEST_ADD1(make_big(ctx, 1, 1, 1), make_big(ctx, 1), make_big(ctx, 2, 1, 1));
  TEST_ADD1(make_big_literal(ctx, {1, 1, 1}), make_big_literal(ctx, {limb_max}), make_big_literal(ctx, {0, 2, 1}));

#undef TEST_ADD1

#define TEST_ADD2(lhs, rhs, result) EXPECT_TRUE(num_equal(add(ctx, lhs, rhs), result))

  TEST_ADD2(make_small(integer::max), make_small(1), make_big(ctx, integer::max + 1));
  TEST_ADD2(make_small(integer::max / 2 + 1), make_small(integer::max / 2 + 1),
            make_big(ctx, 2 * (integer::max / 2 + 1)));
  TEST_ADD2(make_small(integer::min), make_small(-1), make_big_negative(ctx, -integer::min + 1));
  TEST_ADD2(make_big_negative_literal(ctx, {0, 1}), make_big_literal(ctx, {0, 2}), make_big_literal(ctx, {0, 1}));
  TEST_ADD2(make_big_literal(ctx, {0, 2}), make_big_negative_literal(ctx, {0, 1}), make_big_literal(ctx, {0, 1}));
  TEST_ADD2(make_big_negative(ctx, 0, 1), make_big_negative(ctx, 0, 2), make_big_negative(ctx, 0, 3));

#undef TEST_ADD2

#define TEST_SUB(lhs, rhs, result) EXPECT_TRUE(num_equal(subtract(ctx, lhs, rhs), result))

  TEST_SUB(make_big(ctx, 7), make_big(ctx, 5), make_big(ctx, 2));
  TEST_SUB(make_big(ctx, 5), make_big(ctx, 7), make_big_negative(ctx, 2));
  TEST_SUB(make_big(ctx, 0), make_big(ctx, 2), make_big_negative(ctx, 2));
  TEST_SUB(make_big_literal(ctx, {0, 1}), make_small(1), make_big_literal(ctx, {limb_max}));
  TEST_SUB(make_small(1), make_big_literal(ctx, {0, 1}), make_big_negative_literal(ctx, {limb_max}));
  TEST_SUB(make_small(integer::min), make_small(1), make_big_negative(ctx, -integer::min + 1));
  TEST_SUB(make_small(integer::max), make_small(-1), make_big(ctx, integer::max + 1));

#undef TEST_SUB
}

TEST_F(numeric, bignum_multiply) {
  auto make_small = [&] (integer::value_type v) {
    return integer_to_ptr(integer{v});
  };

#define TEST_MUL(lhs, rhs, result) EXPECT_TRUE(num_equal(multiply(ctx, lhs, rhs), result))

  TEST_MUL(make_big(ctx, 6), make_big(ctx, 4), make_big(ctx, 24));
  TEST_MUL(make_big(ctx, 3, 7), make_big(ctx, 2), make_big(ctx, 6, 14));
  TEST_MUL(make_big_literal(ctx, {limb_max / 2 + 2}), make_small(2), make_big_literal(ctx, {2, 1}));
  TEST_MUL(make_big(ctx, 0xAAAAAAAAAAAAAAAA), make_big(ctx, 0x5555555555555555),
           make_big(ctx, 2049638230412172402, 4099276460824344803));
  TEST_MUL(make_big_literal(ctx, {limb_max, limb_max, limb_max}),
           make_big_literal(ctx, {limb_max, limb_max, limb_max}),
           make_big_literal(ctx, {1ull, 0ull, 0ull, limb_max - 1, limb_max, limb_max}));
  TEST_MUL(make_big(ctx,
                    7553641000729792380ull,
                    5922650218298786245ull,
                    16162713787851717198ull,
                    10217089460051462907ull,
                    8909038635035976174ull,
                    6264544477583426584ull),
           make_big(ctx,
                    135006098906616526ull,
                    18228197287099656848ull,
                    16295224771980191197ull,
                    12041080681835578308ull,
                    12088442273849314669ull,
                    16369766287213198900ull),
           make_big(ctx,
                    17587943455618018760ull,
                    3661188350774644697ull,
                    18134422418394596149ull,
                    2811448761515084749ull,
                    9308728024445826184ull,
                    13579246687949292473ull,
                    14227902833484535106ull,
                    9980069117625531926ull,
                    17630642390782412258ull,
                    10715135489352511738ull,
                    2172624792098790866ull,
                    5559199422083740143ull));
  TEST_MUL(make_big(ctx,
                    4973457347152855529ull,
                    3974748182163407329ull,
                    12985577770049413009ull,
                    9076302685846177862ull,
                    738070451437927480ull,
                    15264537084396607285ull),
           make_big(ctx,
                    18446486188639752782ull,
                    3232568627881589338ull,
                    6067942679178015607ull,
                    5102215575270724457ull,
                    9073736952742515913ull,
                    1132841502366999848ull),
           make_big(ctx,
                    10907754461151885054ull,
                    17739271069039180747ull,
                    43421228727999234ull,
                    15577091673733669039ull,
                    16779690354161498684ull,
                    8434430878092964694ull,
                    5379906370098889887ull,
                    3835003850008530511ull,
                    10897188578878994922ull,
                    9109306920291096961ull,
                    4303885782369079147ull,
                    937417522275367995ull));
  TEST_MUL(make_big(ctx,
                    7198000039145371562ull,
                    1123912303584447440ull,
                    3980891719142245558ull,
                    8577746048298875858ull,
                    8311727073236976024ull,
                    17884054197143250996ull),
           make_big(ctx,
                    1188782814227785944ull,
                    2325270131132468069ull,
                    16644864265523681630ull,
                    1350302678026269136ull,
                    2920890536911698555ull,
                    14188063578955447128ull),
           make_big(ctx,
                    2755251443097077616ull,
                    3689811377661473058ull,
                    8877032773261469093ull,
                    3501002863683630417ull,
                    3353221143724972381ull,
                    12641577925382422259ull,
                    4772326402676056ull,
                    13270911720695667193ull,
                    1977996037122781734ull,
                    9541927886011288034ull,
                    10643155227105218129ull,
                    13755278274835822806ull));
  TEST_MUL(make_big(ctx,
                    14406138149647546023ull,
                    10014334381816322851ull,
                    11337790629485301035ull,
                    4430716325805898191ull,
                    194131313579415733ull,
                    15917838048752608414ull),
           make_big(ctx,
                    14581525398179601359ull,
                    16070399142870265493ull,
                    2186843138437186843ull,
                    4427149851635696604ull,
                    16869355141075854150ull,
                    7241713424351240708ull),
           make_big(ctx,
                    14269956649823456777ull,
                    12265344471916286582ull,
                    11559624670299181550ull,
                    17455721097159657918ull,
                    12556885120134179191ull,
                    10585635572377579879ull,
                    16816908444647058668ull,
                    15251982720436126217ull,
                    7735126288172378743ull,
                    2550247449105320530ull,
                    17493571027862026555ull,
                    6248930490047179004ull));
  TEST_MUL(make_small(limb_max / 8), make_small(16), make_big_literal(ctx, {limb_max - 15, 1}));
  TEST_MUL(make_small(limb_max / 8), make_small(-16), make_big_negative_literal(ctx, {limb_max - 15, 1}));
  TEST_MUL(make_small(-(limb_max / 8)), make_small(16), make_big_negative_literal(ctx, {limb_max - 15, 1}));
  TEST_MUL(make_small(-(limb_max / 8)), make_small(-16), make_big_literal(ctx, {limb_max - 15, 1}));

#undef TEST_MUL
}

TEST_F(numeric, bignum_divide) {
  auto make_small = [&] (integer::value_type v) {
    return integer_to_ptr(integer{v});
  };

  auto test_div = [&] (ptr<> x, ptr<> y,
                       ptr<> quotient, ptr<> remainder) {
    auto [q, r] = quotient_remainder(ctx, x, y);
    EXPECT_TRUE(num_equal(q, quotient));
    EXPECT_TRUE(num_equal(r, remainder));
  };

  test_div(make_big(ctx, 0, 0, 8), make_small(2), make_big(ctx, 0, 0, 4), make_small(0));
  test_div(make_big(ctx, 1, 0, 8), make_small(2), make_big(ctx, 0, 0, 4), make_small(1));
  test_div(make_big(ctx, 0, 0, 9), make_small(2), make_big(ctx, 0, 9223372036854775808ull, 4), make_small(0));
  test_div(make<big_integer>(ctx, limb_vector{1, 0, 0, limb_max - 1, limb_max, limb_max}),
           make<big_integer>(ctx, limb_vector{limb_max, limb_max, limb_max}),
           make<big_integer>(ctx, limb_vector{limb_max, limb_max, limb_max}),
           make_small(0));
  test_div(make<big_integer>(ctx, limb_vector{limb_max, limb_max}),
           make<big_integer>(ctx, limb_vector{2, limb_max}),
           make_small(1),
           make<big_integer>(ctx, limb_vector{limb_max - 2}));
  test_div(make_small(-5), make_small(2), make_small(-2), make_small(-1));
  test_div(make_small(5), make_small(-2), make_small(-2), make_small(1));
  test_div(make_small(-5), make_small(-2), make_small(2), make_small(-1));
  test_div(make_big(ctx,
                    14874543083359811318ull,
                    1935678593982463049ull,
                    13199980569319760333ull,
                    15410505428270988819ull,
                    10213726445258162469ull,
                    12395278556799578418ull,
                    7456403789386227136ull,
                    1076335716858975346ull,
                    10881479039968020671ull),
           make_big(ctx,
                    11194330546299954062ull,
                    8087252728553791077ull,
                    7865184323385124607ull,
                    2510237557858698741ull,
                    6408985391941973001ull,
                    11839946699381125055ull,
                    10961764771554828402ull,
                    6611077199486813791ull,
                    2901902384916741495ull),
           make_small(3),
           make_big(ctx,
                    18185039591879052364ull,
                    14567408555740193048ull,
                    8051171672873938126ull,
                    7879792754694892595ull,
                    9433514343141795082ull,
                    13768926606075306484ull,
                    11464597622140845160ull,
                    18136592265817637203ull,
                    2175771885217796184ull));

  test_div(make_big(ctx,
                    7692261422231040055ull,
                    15384495960622693763ull,
                    8772732661969891694ull,
                    2535977428667197115ull,
                    16174420715838619803ull,
                    3874780564069069863ull,
                    10705952965053044654ull,
                    8482589844543901272ull,
                    8164205199812095894ull),
           make_big(ctx,
                    7282122855066198906ull,
                    8675704914817105958ull,
                    8550653847546750756ull,
                    15565807313993354545ull,
                    8244894239515059345ull,
                    12359625180438533263ull,
                    6526454882856490956ull,
                    3550628387050239179ull,
                    4768174037124105332ull),
           make_big(ctx, 1ull),
           make_big(ctx,
                    410138567164841149ull,
                    6708791045805587805ull,
                    222078814423140938ull,
                    5416914188383394186ull,
                    7929526476323560457ull,
                    9961899457340088216ull,
                    4179498082196553697ull,
                    4931961457493662093ull,
                    3396031162687990562ull));

  test_div(make_big(ctx,
                    2446330396973426494ull,
                    14461425362529891049ull,
                    4366362775501768695ull,
                    4235920909984245928ull,
                    16769734018257183108ull,
                    15925757854589919735ull,
                    7341355787832885332ull,
                    10348492034933946425ull,
                    13731135294911915075ull),
           make_big(ctx,
                    13880604877705042050ull,
                    2611974763742920252ull,
                    12134750319276141604ull,
                    4688483100727472282ull,
                    18258228925628760636ull,
                    4887347349206545042ull,
                    4787504742638920873ull,
                    14685252780079427324ull,
                    9435378219307584956ull),
           make_small(1),
           make_big(ctx,
                    7012469592977936060ull,
                    11849450598786970796ull,
                    10678356529935178707ull,
                    17994181882966325261ull,
                    16958249166337974087ull,
                    11038410505383374692ull,
                    2553851045193964459ull,
                    14109983328564070717ull,
                    4295757075604330118ull));

  test_div(make_big(ctx,
                    12506852871317207676ull,
                    10700938666703083024ull,
                    12626603003370626477ull,
                    14261716924607286084ull,
                    2161370295798928489ull,
                    10122529076047113631ull,
                    16373459737971821544ull,
                    10311005268424454975ull,
                    1100495285425557724ull),
           make_big(ctx,
                    12465178309255449209ull,
                    1427445383266802948ull,
                    5756920205309277496ull,
                    7696573369463630995ull,
                    7566067571733492416ull,
                    13073773422555002889ull,
                    6551708513246522343ull,
                    17329835614592961126ull,
                    14847207217269943192ull),
           make_small(0),
           make_big(ctx,
                    12506852871317207676ull,
                    10700938666703083024ull,
                    12626603003370626477ull,
                    14261716924607286084ull,
                    2161370295798928489ull,
                    10122529076047113631ull,
                    16373459737971821544ull,
                    10311005268424454975ull,
                    1100495285425557724ull));

  test_div(make_big(ctx,
                    13768515950283912958ull,
                    5039599389847194458ull,
                    12107044372400000832ull,
                    134123983447138649ull,
                    2153789414069619898ull,
                    7910490151013994715ull,
                    16554460799871884867ull,
                    13004393950989746279ull,
                    17606722284703736159ull),
           make_big(ctx,
                    13528201411004224237ull,
                    9721197764555830598ull,
                    4888196661345244575ull,
                    2313115633983103010ull,
                    13500977888171292075ull,
                    5759984481056258966ull,
                    12748738408593844646ull,
                    16600874190120711725ull,
                    11498829053734232490ull),
           make_small(1),
           make_big(ctx,
                    240314539279688721ull,
                    13765145699000915476ull,
                    7218847711054756256ull,
                    16267752423173587255ull,
                    7099555599607879438ull,
                    2150505669957735748ull,
                    3805722391278040221ull,
                    14850263834578586170ull,
                    6107893230969503668ull));

  test_div(make_big(ctx,
                    6972646283730170953ull,
                    7519107045762773052ull,
                    4872916222398852026ull,
                    2189920864793289825ull,
                    1602617798560111769ull,
                    10146934531826883440ull,
                    9150028946074351450ull,
                    2209844256419692506ull,
                    2371063704493274143ull),
           make_big(ctx,
                    10108171535437529585ull,
                    1183060610282380979ull,
                    17553143743327730375ull,
                    2571022672643327199ull),
           make_big(ctx,
                    8518718272992650627ull,
                    15809441883104914544ull,
                    13990766892202295253ull,
                    4433784788950460276ull,
                    17012065200607766589ull),
           make_big(ctx,
                    5328558974349566198ull,
                    8640356290420700764ull,
                    7580956359732994985ull,
                    1045752718567588000ull));

  test_div(make_big(ctx,
                    10170782082354554675ull,
                    7960934355353996918ull,
                    5724955629544235019ull,
                    2930662393834309931ull,
                    8140058148974841898ull,
                    3338167932104888346ull,
                    8168977224709667766ull,
                    13173537970649653350ull,
                    5448979766364583280ull),
           make_big(ctx,
                    16552174762367217981ull,
                    17366052293534881471ull,
                    2156616536430823413ull,
                    13732808403092740360ull),
           make_big(ctx,
                    1368660787543327668ull,
                    1158922548137579624ull,
                    1206006948639191786ull,
                    16985513432125315283ull,
                    7319401266117724767ull),
           make_big(ctx,
                    5555724399352713551ull,
                    2855470964291304281ull,
                    1391062671460447334ull,
                    6816471574359062907ull));

  test_div(make_big(ctx,
                    4705319185493022819ull,
                    10937512369564114100ull,
                    4225858901270008466ull,
                    16934837836058328188ull,
                    11345351083103042896ull,
                    12502877840311511405ull,
                    15414710077957670839ull,
                    14073218093612395962ull,
                    10141262211251290976ull),
           make_big(ctx,
                    16365587332934573297ull,
                    14110451132936902545ull,
                    14227516877567820616ull,
                    773047176899413847ull),
           make_big(ctx,
                    4027645462635810675ull,
                    15784796192978711547ull,
                    5351585159487159110ull,
                    13295712082196645282ull,
                    2186960985928611038ull,
                    13ull),
           make_big(ctx,
                    11528601757446224160ull,
                    13150424515817646992ull,
                    12367838195135482048ull,
                    731834574828767176ull));
}

TEST_F(numeric, gcd) {
  EXPECT_EQ(expect<integer>(gcd(ctx, integer_to_ptr(integer{32}), integer_to_ptr(integer{36}))).value(), 4);
  EXPECT_EQ(expect<integer>(gcd(ctx, integer_to_ptr(integer{32}), integer_to_ptr(integer{-36}))).value(), 4);
  EXPECT_EQ(expect<integer>(gcd(ctx,
                                read("15637276472805114870656615051104685548619384683912883036250789338440357844169168108881159530367194070722099976990439"),
                                read("28310636747421372819581491222259960704337320575997318939278431382334585462782549354990068622867945599916809865521754"))).value(),
            3);
  EXPECT_EQ(expect<integer>(gcd(ctx,
                                read("32900744989775849384444111067279827681464964267754061354276182079922022805047611540836777891564097053008952910818635"),
                                read("16871863743058363314072331379289859763122718194749875170705775524655845186234765954680571628747994726760230873387371"))).value(),
            1);
  EXPECT_TRUE(num_equal(gcd(ctx,
                            read("326842357047048580094685541896229290526226710742342560706866927058691036387550824695609726546021742995306480127227041503526172382597364126586477735162986"),
                            read("7143737363507851466671560831127318663187019217037069553424396890442458422936353440819622600793958827154371382539989874611891068389366589075003540538150224")),
                        read("3482687899064411289424507725617653109781215164227824305838")));
}

TEST_F(numeric, fraction_arithmetic) {
  EXPECT_TRUE(num_equal(add(ctx, make_fraction(1, 2), make_fraction(1, 3)), make_fraction(5, 6)));
  EXPECT_TRUE(num_equal(add(ctx, make_fraction(7, 12), make_fraction(5, 12)), integer_to_ptr(integer{1})));
  EXPECT_TRUE(num_equal(add(ctx, make_fraction(1, 6), make_fraction(2, 3)), make_fraction(5, 6)));
  EXPECT_TRUE(num_equal(add(ctx, make_fraction(3, 4), make_fraction(1, 6)), make_fraction(11, 12)));
  EXPECT_TRUE(num_equal(subtract(ctx, integer_to_ptr(integer{1}), make_fraction(1, 3)), make_fraction(2, 3)));
  EXPECT_TRUE(num_equal(subtract(ctx, make_fraction(7, 2), integer_to_ptr(integer{4})), make_fraction(-1, 2)));
  EXPECT_TRUE(num_equal(multiply(ctx, make_fraction(1, 3), make_fraction(2, 3)), make_fraction(2, 9)));
  EXPECT_TRUE(num_equal(multiply(ctx, make_fraction(7, 2), make_fraction(2, 7)), integer_to_ptr(integer{1})));
  EXPECT_TRUE(num_equal(divide(ctx, integer_to_ptr(integer{1}), integer_to_ptr(integer{2})), make_fraction(1, 2)));
  EXPECT_TRUE(num_equal(divide(ctx, integer_to_ptr(integer{8}), integer_to_ptr(integer{2})), integer_to_ptr(integer{4})));
  EXPECT_TRUE(num_equal(divide(ctx, make_fraction(3, 4), make_fraction(2, 3)), make_fraction(9, 8)));
  EXPECT_TRUE(num_equal(divide(ctx, make_fraction(1, 3), make_fraction(2, 3)), make_fraction(1, 2)));
}

TEST_F(numeric, float_arithmetic) {
#define ASSERT_FP_EQ(lhs, rhs) ASSERT_DOUBLE_EQ(expect<floating_point>(lhs)->value, rhs)
  ASSERT_FP_EQ(add(ctx, make<floating_point>(ctx, 0.5), make<floating_point>(ctx, 0.4)), 0.9);
  ASSERT_FP_EQ(add(ctx, make<floating_point>(ctx, 0.7), integer_to_ptr(integer{2})), 2.7);
  ASSERT_FP_EQ(add(ctx, make<floating_point>(ctx, 1.0), make_fraction(1, 2)), 1.5);
  ASSERT_FP_EQ(multiply(ctx, make<floating_point>(ctx, 3.0), make<floating_point>(ctx, 0.5)), 1.5);
  ASSERT_FP_EQ(divide(ctx, make<floating_point>(ctx, 3.0), make<floating_point>(ctx, 2.0)), 1.5);
  ASSERT_FP_EQ(divide(ctx, make<floating_point>(ctx, 1.0), make<floating_point>(ctx, 0.0)),
               floating_point::positive_infinity);
  ASSERT_TRUE(std::isnan(expect<floating_point>(divide(ctx, make<floating_point>(ctx, 0.0),
                                                       make<floating_point>(ctx, 0.0)))->value));
#undef ASSERT_FP_EQ
}

TEST_F(numeric, integer_eqv) {
  auto i1 = read("326842357047048580094685541896229290526226710742342560706866927058691036387550824695609726546021742995306480127227041503526172382597364126586477735162986");
  auto i2 = read("326842357047048580094685541896229290526226710742342560706866927058691036387550824695609726546021742995306480127227041503526172382597364126586477735162986");
  EXPECT_NE(i1, i2);
  EXPECT_NE(object_hash(i1), object_hash(i2));
  EXPECT_TRUE(eqv(ctx, i1, i2));
  EXPECT_EQ(hasheqv(i1), hasheqv(i2));
}

TEST_F(numeric, is_integer) {
  EXPECT_TRUE(is_integer(read("2")));
  EXPECT_TRUE(is_integer(read("2.0")));
  EXPECT_TRUE(is_integer(read("4/2")));
}

TEST_F(numeric, bit_length) {
  EXPECT_EQ(bit_length(ctx, integer_to_ptr(0)), 0);
  EXPECT_EQ(bit_length(ctx, integer_to_ptr(1)), 1);
  EXPECT_EQ(bit_length(ctx, integer_to_ptr(2)), 2);
  EXPECT_EQ(bit_length(ctx, integer_to_ptr(5)), 3);

  EXPECT_EQ(bit_length(ctx, integer_to_ptr(-1)), 0);
  EXPECT_EQ(bit_length(ctx, integer_to_ptr(-4)), 2);
  EXPECT_EQ(bit_length(ctx, integer_to_ptr(-7)), 3);
  EXPECT_EQ(bit_length(ctx, integer_to_ptr(-8)), 3);

  EXPECT_EQ(bit_length(ctx, read("340282366920938463463374607431768211456")), 129);
  EXPECT_EQ(bit_length(ctx, read("340282366920938463463374607431768211457")), 129);
  EXPECT_EQ(bit_length(ctx, read("-340282366920938463463374607431768211456")), 128);
  EXPECT_EQ(bit_length(ctx, read("-340282366920938463463374607431768211457")), 129);
}

TEST_F(numeric, inexact) {
  EXPECT_DOUBLE_EQ(expect<floating_point>(inexact(ctx, integer_to_ptr(2)))->value, 2.0);
  EXPECT_DOUBLE_EQ(expect<floating_point>(inexact(ctx, integer_to_ptr(-2)))->value, -2.0);
  EXPECT_DOUBLE_EQ(expect<floating_point>(inexact(ctx, integer_to_ptr(4611686018427387903)))->value, 4611686018427387903.0);

  EXPECT_DOUBLE_EQ(expect<floating_point>(inexact(ctx, read("1237940039285380274899124224")))->value, 1237940039285380274899124224.0);
  EXPECT_DOUBLE_EQ(expect<floating_point>(inexact(ctx, read("89884656743115795386465259539451236680898848947115328636715040578866337902750481566354238661203768010560056939935696678829394884407208311246423715319737062188883946712432742638151109800623047059726541476042502884419075341171231440736956555270413618581675255342293149119973622969239858152417678164812112068608")))->value,
                   89884656743115795386465259539451236680898848947115328636715040578866337902750481566354238661203768010560056939935696678829394884407208311246423715319737062188883946712432742638151109800623047059726541476042502884419075341171231440736956555270413618581675255342293149119973622969239858152417678164812112068608.0);
  EXPECT_THROW(inexact(ctx, read("179769313486231590772930519078902473361797697894230657273430081157732675805500963132708477322407536021120113879871393357658789768814416622492847430639474124377767893424865485276302219601246094119453082952085005768838150682342462881473913110540827237163350510684586298239947245938479716304835356329624224137216")),
               std::runtime_error);

  EXPECT_DOUBLE_EQ(expect<floating_point>(inexact(ctx, read("1/2")))->value, 0.5);
  EXPECT_DOUBLE_EQ(expect<floating_point>(inexact(ctx, read("1/2")))->value, 0.5);
  EXPECT_DOUBLE_EQ(expect<floating_point>(inexact(ctx, read("1267650600228229401496703205376/1267650600228229401496703205377")))->value, 1.0);
}
