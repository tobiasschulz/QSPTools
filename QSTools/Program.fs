#if INTERACTIVE
#r @"c:\All\Project\Parsers\QSParse\QSParse\bin\Debug\QSParse.exe"
#endif
open QS

module codeTools = 
    let filter func statemetns =
        let rec f acc = function
            | [] -> acc |> List.rev
            | h::t ->
                if func h then f (h::acc) t
                else
                    match h with
                    | Act(x, xs) ->
                        match f [] xs with
                        | [] -> f acc t
                        | xs -> f (Act(x, xs)::acc) t
                    | AssertCode(e, xs) ->
                        match f [] xs with
                        | [] -> f acc t
                        | xs -> f (AssertCode(e, xs)::acc) t
                    | If(x, xs, ys) ->
                        let xs = f [] xs
                        let ys = f [] ys
                        if List.isEmpty xs && List.isEmpty ys then f acc t
                        else f (If(x, xs, ys)::acc) t
                    | _ -> f acc t
        f [] statemetns    
    let filterSimple func statemetns =
        let rec f acc = function
            | [] -> acc |> List.rev
            | h::t ->
                match func h with
                | Some x -> f (x::acc) t
                | None ->
                    match h with
                    | Act(x, xs) -> f (f acc xs) t
                    | AssertCode(e, xs) -> f (f acc xs) t
                    | If(x, xs, ys) -> f (f (f acc xs) ys) t
                    | _ -> f acc t
        f [] statemetns
let sample = 
        If
           (Expr (Eq,Func ("idx",[Var "$ARGS"; Val (Int 0)]),Val (String "start")),
            [FuncS ("CLOSE",[Var "ALL"]);
             If
               (Expr (Eq,Var "sound",Val (Int 0)),
                [FuncS ("PLAY",[Val (String "sound/shop.mp3"); Val (Int 30)])],[]);
             AssertCode
               (Var "$icecreem",
                [If
                   (Expr
                      (And,Expr (Ge,Var "hour",Val (Int 8)),
                       Expr (Le,Var "hour",Val (Int 20))),
                    [FuncS ("cls",[]); Assert (Var "minut",Val (Int 3));
                     StringS "Мороженное предлагают за 50 рублей.";
                     Act
                       ([Val (String "Выйти из отдела")],
                        [FuncS ("gt",[Val (String "shop"); Val (String "start")])]);
                     If
                       (Expr (Ge,Var "money",Val (Int 50)),
                        [Act
                           ([Val (String "Купить мороженное")],
                            [FuncS ("cls",[]); Assert (Var "money",Val (Int 50));
                             Assert (Var "salo",Val (Int 1));
                             Assert (Var "fat",Val (Int 50));
                             Assert (Var "manna",Val (Int 500));
                             Assert (Var "water",Val (Int 20));
                             Assert (Var "energy",Val (Int 20));
                             Assert (Var "perkice",Val (Int 1));
                             FuncS ("gs",[Val (String "stat")]); Comment "icecreem";
                             StringS
                               "<center><img src=\"images/pic/icecreem.jpg\"></center>";
                             StringS "Вы купили мороженное и съели его не отходя далеко.";
                             Act
                               ([Val (String "Выйти из отдела")],
                                [FuncS
                                   ("gt",[Val (String "shop"); Val (String "start")])])])],
                        [])],[StringS "Прилавок с мороженным закрыт"])]);
             AssertCode
               (Var "$sofood",
                [If
                   (Expr
                      (And,Expr (Ge,Var "hour",Val (Int 8)),
                       Expr (Le,Var "hour",Val (Int 20))),
                    [Assert (Var "minut",Val (Int 3));
                     FuncS ("gt",[Val (String "shop"); Val (String "food")])],
                    [StringS "Отдел закрыт"])]);
             AssertCode
               (Var "$socosm",
                [If
                   (Expr
                      (And,Expr (Ge,Var "hour",Val (Int 8)),
                       Expr (Le,Var "hour",Val (Int 20))),
                    [Assert (Var "minut",Val (Int 3));
                     FuncS ("gt",[Val (String "shop"); Val (String "cosm")])],
                    [StringS "Отдел закрыт"])]);
             AssertCode
               (Var "$sohos",
                [If
                   (Expr
                      (And,Expr (Ge,Var "hour",Val (Int 8)),
                       Expr (Le,Var "hour",Val (Int 20))),
                    [Assert (Var "minut",Val (Int 3));
                     FuncS ("gt",[Val (String "shop"); Val (String "hos")])],
                    [StringS "Отдел закрыт"])]);
             AssertCode
               (Var "$soclo",
                [If
                   (Expr
                      (And,Expr (Ge,Var "hour",Val (Int 8)),
                       Expr (Le,Var "hour",Val (Int 20))),
                    [Assert (Var "minut",Val (Int 3));
                     FuncS ("gt",[Val (String "shop"); Val (String "clo")])],
                    [StringS "Отдел закрыт"])]);
             AssertCode
               (Var "$soteh",
                [If
                   (Expr
                      (And,Expr (Ge,Var "hour",Val (Int 8)),
                       Expr (Le,Var "hour",Val (Int 20))),
                    [Assert (Var "minut",Val (Int 3));
                     FuncS ("gt",[Val (String "shop"); Val (String "teh")])],
                    [StringS "Отдел закрыт"])]);
             AssertCode
               (Var "$sobank",
                [If
                   (Expr
                      (And,Expr (Ge,Var "hour",Val (Int 8)),
                       Expr (Le,Var "hour",Val (Int 20))),
                    [If
                       (Expr (Gt,Var "karta",Val (Int 0)),
                        [Act
                           ([Val (String "Снять деньги со счета")],
                            [FuncS ("cla",[]);
                             Assert (Var "minut",Expr (Plus,Var "minut",Val (Int 5)));
                             Assert
                               (Var "kartaOUT",
                                Func
                                  ("input",
                                   [Val
                                      (String "Сколько денег вы хотите снять со счета?")]));
                             If
                               (Expr
                                  (Or,Expr (Le,Var "kartaOUT",Val (Int 0)),
                                   Expr (Gt,Var "kartaOUT",Var "karta")),
                                [StringS "Некорректная операция."],
                                [Assert
                                   (Var "karta",Expr (Minus,Var "karta",Var "kartaOUT"));
                                 Assert
                                   (Var "money",Expr (Plus,Var "money",Var "kartaOUT"));
                                 StringS
                                   "Вы сняли со счета <<kartaOUT>> рублей, теперь на вашем счету <<karta>> рублей."]);
                             Act
                               ([Val (String "Отойти")],
                                [FuncS
                                   ("gt",[Val (String "shop"); Val (String "start")])])])],
                        [StringS "У вас нет банковской карточки"])],
                    [StringS "Банкомат отключен"])]); FuncS ("cla",[]); FuncS ("*clr",[]);
             Assert (Var "minut",Expr (Plus,Var "minut",Val (Int 1))); FuncS ("clr",[]);
             FuncS ("gs",[Val (String "stat")]);
             Assert (Var "FColor",Func ("RGB",[Val (Int 0); Val (Int 0); Val (Int 0)]));
             Assert
               (Var "BColor",Func ("RGB",[Val (Int 255); Val (Int 255); Val (Int 255)]));
             Assert
               (Var "LColor",Func ("RGB",[Val (Int 106); Val (Int 90); Val (Int 205)]));
             StringS "<center><b><font color = maroon>Супермаркет</font></b></center>";
             StringS "<center><img src=\"images/pic/shop.jpg\"></center>";
             StringS "Салон связи <a href=\"exec:GT ''shop'',''megafon''\">Мегафон</a>";
             StringS
               "В холле стоит <a href=\"exec: dynamic $sobank \">банкомат</a>, с которого можно снять деньги если конечно они у вас есть на счету в банке.";
             StringS
               "Самое большое место в супермаркете занимает <a href=\"exec: dynamic $sofood \">продуктовый отдел</a>, но в магазине есть еще отделы <a href=\"exec: dynamic $socosm \">косметики</a>, <a href=\"exec: dynamic $sohos \">хозяйственный</a>, <a href=\"exec: dynamic $soclo \">одежды</a>, и отдел <a href=\"exec: dynamic $soteh \">бытовой техники</a>. Не далеко от входа стоит <a href=\"exec: dynamic $icecreem \">прилавок с мороженным</a>";
             If
               (Expr (Eq,Var "$loc",Val (String "gorodok")),
                [If
                   (Expr (Lt,Var "week",Val (Int 6)),
                    [If
                       (Expr
                          (And,Expr (Ge,Var "hour",Val (Int 8)),
                           Expr (Lt,Var "hour",Val (Int 16))),
                        [StringS
                           "Ваша сестра <a href=\"exec:SiSonWork = 1 & GT ''sister''\">Аня</a> сидит на кассе."],
                        [])],[])],[]);
             If
               (Expr (Eq,Var "$loc",Val (String "street")),
                [Assert (Var "evrand",Func ("RAND",[Val (Int 1); Val (Int 100)]));
                 If
                   (Expr
                      (And,Expr (Ge,Var "evrand",Val (Int 95)),
                       Expr (Eq,Var "dimaQW",Val (Int 0))),
                    [StringS
                       "В холле магазина на вас оценивающе смотрит какой то мужчина.";
                     Act
                       ([Val (String "Смотреть на него")],
                        [FuncS ("gt",[Val (String "event"); Val (String "dima")])])],
                    [If
                       (Expr
                          (And,Expr (Ge,Var "evrand",Val (Int 95)),
                           Expr (Eq,Var "dimaQW",Val (Int 1))),
                        [StringS
                           "В холле магазина вы замечаете Диму и вас сковывает страх, вы не можете пошевелиться.";
                         Act
                           ([Val (String "Смотреть на него")],
                            [FuncS
                               ("gt",[Val (String "event"); Val (String "scoreslut1")])])],
                        [If
                           (Expr
                              (And,Expr (Ge,Var "evrand",Val (Int 95)),
                               Expr (Eq,Var "dimaQW",Val (Int 2))),
                            [StringS
                               "Вам встретился тот самый Дима который с дружком развлекался трахая вас. Дима с самоуверенной усмешкой подошел к вам и поздоровался.";
                             Act
                               ([Val (String "Поздороваться")],
                                [FuncS ("cla",[]);
                                 StringS
                                   "Пойдем ко мне, у меня жены дома нет сказал Дима.";
                                 Act
                                   ([Val (String "Нет")],
                                    [FuncS
                                       ("gt",[Val (String "shop"); Val (String "start")])]);
                                 Act
                                   ([Val (String "Пойдем")],
                                    [FuncS ("cla",[]); FuncS ("*clr",[]);
                                     Assert
                                       (Var "gostrand",
                                        Func ("RAND",[Val (Int 1); Val (Int 2)]));
                                     If
                                       (Expr (Eq,Var "gostrand",Val (Int 1)),
                                        [FuncS ("cla",[]);
                                         Assert
                                           (Var "sex",Expr (Plus,Var "sex",Val (Int 1)));
                                         Assert
                                           (Var "oral",
                                            Expr (Plus,Var "oral",Val (Int 1)));
                                         Assert
                                           (Var "throat",
                                            Expr (Plus,Var "throat",Val (Int 1)));
                                         Assert
                                           (Var "cumlip",
                                            Expr (Plus,Var "cumlip",Val (Int 1)));
                                         Assert
                                           (Var "swallow",
                                            Expr (Plus,Var "swallow",Val (Int 1)));
                                         Assert (Var "horny",Val (Int 0));
                                         StringS
                                           "Дима отвел вас к себе домой и там приказал вам встать на колени. Вы встали на колени и он называя вас сучкой и хуесоской стал трахать вас в рот.";
                                         StringS
                                           "<center><img src=\"images/pics/gostDT2.jpg\"></center>";
                                         StringS
                                           "Наконец то Дима кончил вам в рот, вы все послушно проглотили и слизали остатки спермы с его большого члена.";
                                         Act
                                           ([Val (String "Идти домой")],
                                            [FuncS ("gt",[Val (String "street")])])],
                                        [If
                                           (Expr (Eq,Var "gostrand",Val (Int 2)),
                                            [FuncS ("cla",[]);
                                             Assert
                                               (Var "sex",
                                                Expr (Plus,Var "sex",Val (Int 1)));
                                             Assert
                                               (Var "anal",
                                                Expr (Plus,Var "anal",Val (Int 1)));
                                             Assert
                                               (Var "cumass",
                                                Expr (Plus,Var "cumass",Val (Int 1)));
                                             Assert
                                               (Var "spank",
                                                Expr (Plus,Var "spank",Val (Int 1)));
                                             Assert (Var "boom",Val (Int 5));
                                             Assert (Var "horny",Val (Int 0));
                                             StringS
                                               "Дима привел вас к себе домой и прямо в коридоре начал вас целовать и раздевать. После того как он раздел вас он отвел вас в ванную. Там он усадил вас в ванную и открутив лейку душа вставил шланг с водой вам в зад. Быстро возникло чувство наполненности и Дима посадил вас на унитаз. После клизмы он поставил вас раком и вставил вам в анус свой большой член.";
                                             StringS
                                               "<center><img src=\"images/pics/gostAnal2.jpg\"></center>";
                                             StringS
                                               "Дима долго вас трахал и бил по заднице ладонями, вам сначала было больно, но постепенно вы вошли во вкус и начали ловить кайф, наконец ваша попка наполнилась горячей жидкостью и Дима застонав схватил ваши ягодицы.";
                                             Act
                                               ([Val (String "Идти домой")],
                                                [FuncS ("gt",[Val (String "street")])])],
                                            [])])])])],[])])])],[]);
             If
               (Expr
                  (And,Expr (Ge,Var "hour",Val (Int 8)),
                   Expr (Le,Var "hour",Val (Int 20))),
                [StringS "В магазине толпы покупателей.";
                 If
                   (Expr (Gt,Var "TorgPredZ",Val (Int 0)),
                    [If
                       (Expr (Eq,Var "$loc",Val (String "street")),
                        [If
                           (Expr (Ne,Var "StreetShopTPday",Var "day"),
                            [Act
                               ([Val (String "Расставлять продукцию")],
                                [FuncS ("cls",[]);
                                 Assert
                                   (Var "mtprand",
                                    Func ("RAND",[Val (Int 40); Val (Int 130)]));
                                 Assert (Var "minut",Var "mtprand");
                                 Assert (Var "TorgPredZ",Val (Int 1));
                                 Assert (Var "TorgPredZV",Val (Int 1));
                                 Assert (Var "StreetShopTPday",Var "day");
                                 FuncS ("gs",[Val (String "stat")]);
                                 StringS
                                   "<center><img src=\"images/pic/shop.jpg\"></center>";
                                 StringS
                                   "Вы расставляли продукцию довольно долгое время и когда наконец справились, то пошли на кассу и вам там пробили чек.";
                                 Act
                                   ([Val (String "Выйти")],
                                    [FuncS
                                       ("gt",[Val (String "shop"); Val (String "start")])])])],
                            [])],
                        [If
                           (Expr (Eq,Var "$loc",Val (String "Nord")),
                            [If
                               (Expr (Ne,Var "nordShopTPday",Var "day"),
                                [Act
                                   ([Val (String "Расставлять продукцию")],
                                    [FuncS ("cls",[]);
                                     Assert
                                       (Var "mtprand",
                                        Func ("RAND",[Val (Int 40); Val (Int 130)]));
                                     Assert (Var "minut",Var "mtprand");
                                     Assert (Var "TorgPredZ",Val (Int 1));
                                     Assert (Var "TorgPredZV",Val (Int 1));
                                     Assert (Var "nordShopTPday",Var "day");
                                     FuncS ("gs",[Val (String "stat")]);
                                     StringS
                                       "<center><img src=\"images/pic/shop.jpg\"></center>";
                                     StringS
                                       "Вы расставляли продукцию довольно долгое время и когда наконец справились, то пошли на кассу и вам там пробили чек.";
                                     Act
                                       ([Val (String "Выйти")],
                                        [FuncS
                                           ("gt",
                                            [Val (String "shop"); Val (String "start")])])])],
                                [])],
                            [If
                               (Expr (Eq,Var "$loc",Val (String "down")),
                                [If
                                   (Expr (Ne,Var "downShopTPday",Var "day"),
                                    [Act
                                       ([Val (String "Расставлять продукцию")],
                                        [FuncS ("cls",[]);
                                         Assert
                                           (Var "mtprand",
                                            Func ("RAND",[Val (Int 40); Val (Int 130)]));
                                         Assert (Var "minut",Var "mtprand");
                                         Assert (Var "TorgPredZ",Val (Int 1));
                                         Assert (Var "TorgPredZV",Val (Int 1));
                                         Assert (Var "downShopTPday",Var "day");
                                         FuncS ("gs",[Val (String "stat")]);
                                         StringS
                                           "<center><img src=\"images/pic/shop.jpg\"></center>";
                                         StringS
                                           "Вы расставляли продукцию довольно долгое время и когда наконец справились, то пошли на кассу и вам там пробили чек.";
                                         Act
                                           ([Val (String "Выйти")],
                                            [FuncS
                                               ("gt",
                                                [Val (String "shop");
                                                 Val (String "start")])])])],[])],[])])])],
                    [])],[StringS "Магазин закрыт."]);
             Act ([Val (String "Выйти из Магазина")],[FuncS ("gt",[Var "$loc"])])],[])

let p = Program.parseFile @"c:\All\It\DefaultBox\drive\C\All2\Games\GamesSourceCode\etoEdit.txt"
let filter f = 
    let f = codeTools.filter f
    let res = 
        List.choose (function Location(name, xs) -> 
                              match f xs with [] -> None | xs -> Location(name, f xs) |> QS.printLoc |> Some) p
    System.IO.File.WriteAllLines(@"c:\All\Project\Parsers\QSParse\QSParse\bin\Debug\output.txt", res)

filter (function FuncS(name, _) -> Set.contains name (set["gt"; "goto"; "xgt"; "xgoto"; "gs"; "gosub"]) | _ -> false)
filter (function FuncS("gt",Val (String "office")::_) -> true | _ -> false)
filter (function Assert(_, _) -> true | _ -> false)

Program.parse "'some text' + var + 'some text2'\r\n"
let res2 f = List.choose (function Location(name, xs) -> match codeTools.filterSimple f xs with [] -> None | xs -> Some xs) p

res2 (function StringS _ as x -> Some x | ExprS _ as x -> Some x | _ -> None)
res2 (function AssertCode(name, _) -> AssertCode(name, []) |> Some | _ -> None) |> List.concat
|> Seq.groupBy id
|> Seq.map (fun (key, v) -> key, Seq.length v) |> List.ofSeq |> List.filter (snd >> ((<)2))
