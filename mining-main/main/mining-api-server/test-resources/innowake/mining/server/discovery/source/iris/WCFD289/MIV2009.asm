         PRINT NOGEN                                                    00001000
         CSECT                                                          00001300
         AMODE ANY                                                      00001390
         RMODE ANY                                                      00001480
MAPSET   DFHMSD                                                        X00002000
               TYPE=MAP,                                               X00003000
               CTRL=(FREEKB,FRSET),                                    X00004000
               MODE=INOUT,                                             X00005000
               LANG=COBOL                                               00006000
MIV2009  DFHMDI                                                        X00007000
               SIZE=(24,80),                                           X00008000
               TIOAPFX=YES                                              00009000
         COPY  MIN911                                                   00010000
         DFHMDF                                                        X00011000
               POS=(03,28),                                            X00012000
               ATTRB=ASKIP,                                            X00013000
               INITIAL='**',                                           X00014000
               LENGTH=02                                                00015000
MAPDSC   DFHMDF                                                        X00016000
               POS=(03,31),                                            X00017000
               ATTRB=ASKIP,                                            X00018000
               LENGTH=30                                                00019000
         DFHMDF                                                        X00020000
               POS=(03,62),                                            X00021000
               ATTRB=ASKIP,                                            X00022000
               INITIAL='- Form',                                       X00023000
               LENGTH=06                                                00024000
FRMNBR   DFHMDF                                                        X00025000
               POS=(03,69),                                            X00026000
               ATTRB=ASKIP,                                            X00027000
               LENGTH=04                                                00028000
         DFHMDF                                                        X00029000
               POS=(03,74),                                            X00030000
               ATTRB=ASKIP,                                            X00031000
               INITIAL='**',                                           X00032000
               LENGTH=02                                                00033000
REGLIT   DFHMDF                                                        X00034000
               POS=(04,02),                                            X00035000
               ATTRB=ASKIP,                                            X00036000
               LENGTH=06                                                00037000
REGION   DFHMDF                                                        X00038000
               POS=(04,15),                                            X00039000
               ATTRB=ASKIP,                                            X00040000
               LENGTH=03                                                00041000
         DFHMDF                                                        X00042000
               POS=(04,19),                                            X00043000
               ATTRB=ASKIP,                                            X00044000
               LENGTH=01                                                00045000
EFFLIT   DFHMDF                                                        X00046000
               POS=(04,21),                                            X00047000
               ATTRB=ASKIP,                                            X00048000
               LENGTH=14                                                00049000
EFFMM    DFHMDF                                                        X00050000
               POS=(04,37),                                            X00051000
               ATTRB=ASKIP,                                            X00052000
               LENGTH=02                                                00053000
EFFDD    DFHMDF                                                        X00054000
               POS=(04,40),                                            X00055000
               ATTRB=ASKIP,                                            X00056000
               LENGTH=02                                                00057000
EFFYY    DFHMDF                                                        X00058000
               POS=(04,43),                                            X00059000
               ATTRB=ASKIP,                                            X00060000
               LENGTH=02                                                00061000
         DFHMDF                                                        X00062000
               POS=(04,46),                                            X00063000
               ATTRB=ASKIP,                                            X00064000
               LENGTH=01                                                00065000
MODLIT   DFHMDF                                                        X00066000
               POS=(04,50),                                            X00067000
               ATTRB=ASKIP,                                            X00068000
               LENGTH=05                                                00069000
MODEL    DFHMDF                                                        X00070000
               POS=(04,57),                                            X00071000
               ATTRB=ASKIP,                                            X00072000
               LENGTH=01                                                00073000
         DFHMDF                                                        X00074000
               POS=(04,59),                                            X00075000
               ATTRB=ASKIP,                                            X00076000
               LENGTH=01                                                00077000
DELET    DFHMDF                                                        X00078000
               POS=(04,64),                                            X00079000
               ATTRB=ASKIP,                                            X00080000
               LENGTH=06                                                00081000
DELCDE   DFHMDF                                                        X00082000
               POS=(04,72),                                            X00083000
               ATTRB=ASKIP,                                            X00084000
               LENGTH=01                                                00085000
         DFHMDF                                                        X00086000
               POS=(04,74),                                            X00087000
               ATTRB=ASKIP,                                            X00088000
               INITIAL=' ',                                            X00089000
               LENGTH=01                                                00090000
KEYDSC1  DFHMDF                                                        X00091000
               POS=(05,02),                                            X00092000
               ATTRB=ASKIP,                                            X00093000
               LENGTH=12                                                00094000
KEYFLD1  DFHMDF                                                        X00095000
               POS=(05,15),                                            X00096000
               ATTRB=ASKIP,                                            X00097000
               JUSTIFY=LEFT,                                           X00098000
               LENGTH=12                                                00099000
KEYDSC2  DFHMDF                                                        X00100000
               POS=(05,28),                                            X00101000
               ATTRB=ASKIP,                                            X00102000
               LENGTH=12                                                00103000
KEYFLD2  DFHMDF                                                        X00104000
               POS=(05,41),                                            X00105000
               ATTRB=ASKIP,                                            X00106000
               JUSTIFY=LEFT,                                           X00107000
               LENGTH=12                                                00108000
KEYDSC3  DFHMDF                                                        X00109000
               POS=(05,54),                                            X00110000
               ATTRB=ASKIP,                                            X00111000
               LENGTH=12                                                00112000
KEYFLD3  DFHMDF                                                        X00113000
               POS=(05,67),                                            X00114000
               ATTRB=ASKIP,                                            X00115000
               JUSTIFY=LEFT,                                           X00116000
               LENGTH=12                                                00117000
HEAD1    DFHMDF                                                        X00118000
               POS=(06,15),                                            X00119000
               ATTRB=ASKIP,                                            X00120000
               INITIAL='      ',                                       X00121000
               LENGTH=06                                                00122000
HEAD2    DFHMDF                                                        X00123000
               POS=(06,23),                                            X00124000
               ATTRB=ASKIP,                                            X00125000
               INITIAL='MAINT',                                        X00126000
               LENGTH=05                                                00127000
HEAD3    DFHMDF                                                        X00128000
               POS=(06,30),                                            X00129000
               ATTRB=ASKIP,                                            X00130000
               INITIAL='NEW',                                          X00131000
               LENGTH=03                                                00132000
HEAD4    DFHMDF                                                        X00133000
               POS=(06,35),                                            X00134000
               ATTRB=ASKIP,                                            X00135000
               INITIAL='EMP',                                          X00136000
               LENGTH=03                                                00137000
HEAD5    DFHMDF                                                        X00138000
               POS=(06,40),                                            X00139000
               ATTRB=ASKIP,                                            X00140000
               INITIAL='OFF',                                          X00141000
               LENGTH=03                                                00142000
HEAD6    DFHMDF                                                        X00143000
               POS=(06,45),                                            X00144000
               ATTRB=ASKIP,                                            X00145000
               INITIAL='DIR',                                          X00146000
               LENGTH=03                                                00147000
HEAD7    DFHMDF                                                        X00148000
               POS=(06,50),                                            X00149000
               ATTRB=ASKIP,                                            X00150000
               INITIAL='DORM',                                         X00151000
               LENGTH=04                                                00152000
HEAD8    DFHMDF                                                        X00153000
               POS=(06,56),                                            X00154000
               ATTRB=ASKIP,                                            X00155000
               INITIAL='INACT',                                        X00156000
               LENGTH=05                                                00157000
HEAD9    DFHMDF                                                        X00158000
               POS=(06,63),                                            X00159000
               ATTRB=ASKIP,                                            X00160000
               INITIAL='ESCHEAT',                                      X00161000
               LENGTH=07                                                00162000
DESC1    DFHMDF                                                        X00163000
               POS=(07,01),                                            X00164000
               ATTRB=ASKIP,                                            X00165000
               INITIAL='TRAN CODE 01',                                 X00166000
               LENGTH=12                                                00167000
DTFLD11  DFHMDF                                                        X00168000
               POS=(07,15),                                            X00169000
               ATTRB=ASKIP,                                            X00170000
               JUSTIFY=LEFT,                                           X00171000
               LENGTH=06                                                00172000
         DFHMDF                                                        X00173000
               POS=(07,22),                                            X00174000
               ATTRB=ASKIP,                                            X00175000
               INITIAL=' ',                                            X00176000
               LENGTH=01                                                00177000
DTFLD12  DFHMDF                                                        X00178000
               POS=(07,25),                                            X00179000
               ATTRB=ASKIP,                                            X00180000
               LENGTH=01                                                00181000
         DFHMDF                                                        X00182000
               POS=(07,27),                                            X00183000
               ATTRB=ASKIP,                                            X00184000
               INITIAL=' ',                                            X00185000
               LENGTH=01                                                00186000
DTFLD13  DFHMDF                                                        X00187000
               POS=(07,31),                                            X00188000
               ATTRB=ASKIP,                                            X00189000
               LENGTH=01                                                00190000
         DFHMDF                                                        X00191000
               POS=(07,33),                                            X00192000
               ATTRB=ASKIP,                                            X00193000
               INITIAL=' ',                                            X00194000
               LENGTH=01                                                00195000
DTFLD14  DFHMDF                                                        X00196000
               POS=(07,36),                                            X00197000
               ATTRB=ASKIP,                                            X00198000
               LENGTH=01                                                00199000
         DFHMDF                                                        X00200000
               POS=(07,38),                                            X00201000
               ATTRB=ASKIP,                                            X00202000
               INITIAL=' ',                                            X00203000
               LENGTH=01                                                00204000
DTFLD15  DFHMDF                                                        X00205000
               POS=(07,41),                                            X00206000
               ATTRB=ASKIP,                                            X00207000
               LENGTH=01                                                00208000
         DFHMDF                                                        X00209000
               POS=(07,43),                                            X00210000
               ATTRB=ASKIP,                                            X00211000
               INITIAL=' ',                                            X00212000
               LENGTH=01                                                00213000
DTFLD16  DFHMDF                                                        X00214000
               POS=(07,46),                                            X00215000
               ATTRB=ASKIP,                                            X00216000
               LENGTH=01                                                00217000
         DFHMDF                                                        X00218000
               POS=(07,48),                                            X00219000
               ATTRB=ASKIP,                                            X00220000
               INITIAL=' ',                                            X00221000
               LENGTH=01                                                00222000
DTFLD17  DFHMDF                                                        X00223000
               POS=(07,52),                                            X00224000
               ATTRB=ASKIP,                                            X00225000
               LENGTH=01                                                00226000
         DFHMDF                                                        X00227000
               POS=(07,54),                                            X00228000
               ATTRB=ASKIP,                                            X00229000
               INITIAL=' ',                                            X00230000
               LENGTH=01                                                00231000
DTFLD18  DFHMDF                                                        X00232000
               POS=(07,58),                                            X00233000
               ATTRB=ASKIP,                                            X00234000
               LENGTH=01                                                00235000
         DFHMDF                                                        X00236000
               POS=(07,60),                                            X00237000
               ATTRB=ASKIP,                                            X00238000
               INITIAL=' ',                                            X00239000
               LENGTH=01                                                00240000
DTFLD19  DFHMDF                                                        X00241000
               POS=(07,66),                                            X00242000
               ATTRB=ASKIP,                                            X00243000
               LENGTH=01                                                00244000
         DFHMDF                                                        X00245000
               POS=(07,68),                                            X00246000
               ATTRB=ASKIP,                                            X00247000
               INITIAL=' ',                                            X00248000
               LENGTH=01                                                00249000
DESC2    DFHMDF                                                        X00250000
               POS=(08,01),                                            X00251000
               ATTRB=ASKIP,                                            X00252000
               INITIAL='TRAN CODE 02',                                 X00253000
               LENGTH=12                                                00254000
DTFLD21  DFHMDF                                                        X00255000
               POS=(08,15),                                            X00256000
               ATTRB=ASKIP,                                            X00257000
               JUSTIFY=LEFT,                                           X00258000
               LENGTH=06                                                00259000
         DFHMDF                                                        X00260000
               POS=(08,22),                                            X00261000
               ATTRB=ASKIP,                                            X00262000
               INITIAL=' ',                                            X00263000
               LENGTH=01                                                00264000
DTFLD22  DFHMDF                                                        X00265000
               POS=(08,25),                                            X00266000
               ATTRB=ASKIP,                                            X00267000
               LENGTH=01                                                00268000
         DFHMDF                                                        X00269000
               POS=(08,27),                                            X00270000
               ATTRB=ASKIP,                                            X00271000
               INITIAL=' ',                                            X00272000
               LENGTH=01                                                00273000
DTFLD23  DFHMDF                                                        X00274000
               POS=(08,31),                                            X00275000
               ATTRB=ASKIP,                                            X00276000
               LENGTH=01                                                00277000
         DFHMDF                                                        X00278000
               POS=(08,33),                                            X00279000
               ATTRB=ASKIP,                                            X00280000
               INITIAL=' ',                                            X00281000
               LENGTH=01                                                00282000
DTFLD24  DFHMDF                                                        X00283000
               POS=(08,36),                                            X00284000
               ATTRB=ASKIP,                                            X00285000
               LENGTH=01                                                00286000
         DFHMDF                                                        X00287000
               POS=(08,38),                                            X00288000
               ATTRB=ASKIP,                                            X00289000
               INITIAL=' ',                                            X00290000
               LENGTH=01                                                00291000
DTFLD25  DFHMDF                                                        X00292000
               POS=(08,41),                                            X00293000
               ATTRB=ASKIP,                                            X00294000
               LENGTH=01                                                00295000
         DFHMDF                                                        X00296000
               POS=(08,43),                                            X00297000
               ATTRB=ASKIP,                                            X00298000
               INITIAL=' ',                                            X00299000
               LENGTH=01                                                00300000
DTFLD26  DFHMDF                                                        X00301000
               POS=(08,46),                                            X00302000
               ATTRB=ASKIP,                                            X00303000
               LENGTH=01                                                00304000
         DFHMDF                                                        X00305000
               POS=(08,48),                                            X00306000
               ATTRB=ASKIP,                                            X00307000
               INITIAL=' ',                                            X00308000
               LENGTH=01                                                00309000
DTFLD27  DFHMDF                                                        X00310000
               POS=(08,52),                                            X00311000
               ATTRB=ASKIP,                                            X00312000
               LENGTH=01                                                00313000
         DFHMDF                                                        X00314000
               POS=(08,54),                                            X00315000
               ATTRB=ASKIP,                                            X00316000
               INITIAL=' ',                                            X00317000
               LENGTH=01                                                00318000
DTFLD28  DFHMDF                                                        X00319000
               POS=(08,58),                                            X00320000
               ATTRB=ASKIP,                                            X00321000
               LENGTH=01                                                00322000
         DFHMDF                                                        X00323000
               POS=(08,60),                                            X00324000
               ATTRB=ASKIP,                                            X00325000
               INITIAL=' ',                                            X00326000
               LENGTH=01                                                00327000
DTFLD29  DFHMDF                                                        X00328000
               POS=(08,66),                                            X00329000
               ATTRB=ASKIP,                                            X00330000
               LENGTH=01                                                00331000
         DFHMDF                                                        X00332000
               POS=(08,68),                                            X00333000
               ATTRB=ASKIP,                                            X00334000
               INITIAL=' ',                                            X00335000
               LENGTH=01                                                00336000
DESC3    DFHMDF                                                        X00337000
               POS=(09,01),                                            X00338000
               ATTRB=ASKIP,                                            X00339000
               INITIAL='TRAN CODE 03',                                 X00340000
               LENGTH=12                                                00341000
DTFLD31  DFHMDF                                                        X00342000
               POS=(09,15),                                            X00343000
               ATTRB=ASKIP,                                            X00344000
               JUSTIFY=LEFT,                                           X00345000
               LENGTH=06                                                00346000
         DFHMDF                                                        X00347000
               POS=(09,22),                                            X00348000
               ATTRB=ASKIP,                                            X00349000
               INITIAL=' ',                                            X00350000
               LENGTH=01                                                00351000
DTFLD32  DFHMDF                                                        X00352000
               POS=(09,25),                                            X00353000
               ATTRB=ASKIP,                                            X00354000
               LENGTH=01                                                00355000
         DFHMDF                                                        X00356000
               POS=(09,27),                                            X00357000
               ATTRB=ASKIP,                                            X00358000
               INITIAL=' ',                                            X00359000
               LENGTH=01                                                00360000
DTFLD33  DFHMDF                                                        X00361000
               POS=(09,31),                                            X00362000
               ATTRB=ASKIP,                                            X00363000
               LENGTH=01                                                00364000
         DFHMDF                                                        X00365000
               POS=(09,33),                                            X00366000
               ATTRB=ASKIP,                                            X00367000
               INITIAL=' ',                                            X00368000
               LENGTH=01                                                00369000
DTFLD34  DFHMDF                                                        X00370000
               POS=(09,36),                                            X00371000
               ATTRB=ASKIP,                                            X00372000
               LENGTH=01                                                00373000
         DFHMDF                                                        X00374000
               POS=(09,38),                                            X00375000
               ATTRB=ASKIP,                                            X00376000
               INITIAL=' ',                                            X00377000
               LENGTH=01                                                00378000
DTFLD35  DFHMDF                                                        X00379000
               POS=(09,41),                                            X00380000
               ATTRB=ASKIP,                                            X00381000
               LENGTH=01                                                00382000
         DFHMDF                                                        X00383000
               POS=(09,43),                                            X00384000
               ATTRB=ASKIP,                                            X00385000
               INITIAL=' ',                                            X00386000
               LENGTH=01                                                00387000
DTFLD36  DFHMDF                                                        X00388000
               POS=(09,46),                                            X00389000
               ATTRB=ASKIP,                                            X00390000
               LENGTH=01                                                00391000
         DFHMDF                                                        X00392000
               POS=(09,48),                                            X00393000
               ATTRB=ASKIP,                                            X00394000
               INITIAL=' ',                                            X00395000
               LENGTH=01                                                00396000
DTFLD37  DFHMDF                                                        X00397000
               POS=(09,52),                                            X00398000
               ATTRB=ASKIP,                                            X00399000
               LENGTH=01                                                00400000
         DFHMDF                                                        X00401000
               POS=(09,54),                                            X00402000
               ATTRB=ASKIP,                                            X00403000
               INITIAL=' ',                                            X00404000
               LENGTH=01                                                00405000
DTFLD38  DFHMDF                                                        X00406000
               POS=(09,58),                                            X00407000
               ATTRB=ASKIP,                                            X00408000
               LENGTH=01                                                00409000
         DFHMDF                                                        X00410000
               POS=(09,60),                                            X00411000
               ATTRB=ASKIP,                                            X00412000
               INITIAL=' ',                                            X00413000
               LENGTH=01                                                00414000
DTFLD39  DFHMDF                                                        X00415000
               POS=(09,66),                                            X00416000
               ATTRB=ASKIP,                                            X00417000
               LENGTH=01                                                00418000
         DFHMDF                                                        X00419000
               POS=(09,68),                                            X00420000
               ATTRB=ASKIP,                                            X00421000
               INITIAL=' ',                                            X00422000
               LENGTH=01                                                00423000
DESC4    DFHMDF                                                        X00424000
               POS=(10,01),                                            X00425000
               ATTRB=ASKIP,                                            X00426000
               INITIAL='TRAN CODE 04',                                 X00427000
               LENGTH=12                                                00428000
DTFLD41  DFHMDF                                                        X00429000
               POS=(10,15),                                            X00430000
               ATTRB=ASKIP,                                            X00431000
               JUSTIFY=LEFT,                                           X00432000
               LENGTH=06                                                00433000
         DFHMDF                                                        X00434000
               POS=(10,22),                                            X00435000
               ATTRB=ASKIP,                                            X00436000
               INITIAL=' ',                                            X00437000
               LENGTH=01                                                00438000
DTFLD42  DFHMDF                                                        X00439000
               POS=(10,25),                                            X00440000
               ATTRB=ASKIP,                                            X00441000
               LENGTH=01                                                00442000
         DFHMDF                                                        X00443000
               POS=(10,27),                                            X00444000
               ATTRB=ASKIP,                                            X00445000
               INITIAL=' ',                                            X00446000
               LENGTH=01                                                00447000
DTFLD43  DFHMDF                                                        X00448000
               POS=(10,31),                                            X00449000
               ATTRB=ASKIP,                                            X00450000
               LENGTH=01                                                00451000
         DFHMDF                                                        X00452000
               POS=(10,33),                                            X00453000
               ATTRB=ASKIP,                                            X00454000
               INITIAL=' ',                                            X00455000
               LENGTH=01                                                00456000
DTFLD44  DFHMDF                                                        X00457000
               POS=(10,36),                                            X00458000
               ATTRB=ASKIP,                                            X00459000
               LENGTH=01                                                00460000
         DFHMDF                                                        X00461000
               POS=(10,38),                                            X00462000
               ATTRB=ASKIP,                                            X00463000
               INITIAL=' ',                                            X00464000
               LENGTH=01                                                00465000
DTFLD45  DFHMDF                                                        X00466000
               POS=(10,41),                                            X00467000
               ATTRB=ASKIP,                                            X00468000
               LENGTH=01                                                00469000
         DFHMDF                                                        X00470000
               POS=(10,43),                                            X00471000
               ATTRB=ASKIP,                                            X00472000
               INITIAL=' ',                                            X00473000
               LENGTH=01                                                00474000
DTFLD46  DFHMDF                                                        X00475000
               POS=(10,46),                                            X00476000
               ATTRB=ASKIP,                                            X00477000
               LENGTH=01                                                00478000
         DFHMDF                                                        X00479000
               POS=(10,48),                                            X00480000
               ATTRB=ASKIP,                                            X00481000
               INITIAL=' ',                                            X00482000
               LENGTH=01                                                00483000
DTFLD47  DFHMDF                                                        X00484000
               POS=(10,52),                                            X00485000
               ATTRB=ASKIP,                                            X00486000
               LENGTH=01                                                00487000
         DFHMDF                                                        X00488000
               POS=(10,54),                                            X00489000
               ATTRB=ASKIP,                                            X00490000
               INITIAL=' ',                                            X00491000
               LENGTH=01                                                00492000
DTFLD48  DFHMDF                                                        X00493000
               POS=(10,58),                                            X00494000
               ATTRB=ASKIP,                                            X00495000
               LENGTH=01                                                00496000
         DFHMDF                                                        X00497000
               POS=(10,60),                                            X00498000
               ATTRB=ASKIP,                                            X00499000
               INITIAL=' ',                                            X00500000
               LENGTH=01                                                00501000
DTFLD49  DFHMDF                                                        X00502000
               POS=(10,66),                                            X00503000
               ATTRB=ASKIP,                                            X00504000
               LENGTH=01                                                00505000
         DFHMDF                                                        X00506000
               POS=(10,68),                                            X00507000
               ATTRB=ASKIP,                                            X00508000
               INITIAL=' ',                                            X00509000
               LENGTH=01                                                00510000
DESC5    DFHMDF                                                        X00511000
               POS=(11,01),                                            X00512000
               ATTRB=ASKIP,                                            X00513000
               INITIAL='TRAN CODE 05',                                 X00514000
               LENGTH=12                                                00515000
DTFLD51  DFHMDF                                                        X00516000
               POS=(11,15),                                            X00517000
               ATTRB=ASKIP,                                            X00518000
               JUSTIFY=LEFT,                                           X00519000
               LENGTH=06                                                00520000
         DFHMDF                                                        X00521000
               POS=(11,22),                                            X00522000
               ATTRB=ASKIP,                                            X00523000
               INITIAL=' ',                                            X00524000
               LENGTH=01                                                00525000
DTFLD52  DFHMDF                                                        X00526000
               POS=(11,25),                                            X00527000
               ATTRB=ASKIP,                                            X00528000
               LENGTH=01                                                00529000
         DFHMDF                                                        X00530000
               POS=(11,27),                                            X00531000
               ATTRB=ASKIP,                                            X00532000
               INITIAL=' ',                                            X00533000
               LENGTH=01                                                00534000
DTFLD53  DFHMDF                                                        X00535000
               POS=(11,31),                                            X00536000
               ATTRB=ASKIP,                                            X00537000
               LENGTH=01                                                00538000
         DFHMDF                                                        X00539000
               POS=(11,33),                                            X00540000
               ATTRB=ASKIP,                                            X00541000
               INITIAL=' ',                                            X00542000
               LENGTH=01                                                00543000
DTFLD54  DFHMDF                                                        X00544000
               POS=(11,36),                                            X00545000
               ATTRB=ASKIP,                                            X00546000
               LENGTH=01                                                00547000
         DFHMDF                                                        X00548000
               POS=(11,38),                                            X00549000
               ATTRB=ASKIP,                                            X00550000
               INITIAL=' ',                                            X00551000
               LENGTH=01                                                00552000
DTFLD55  DFHMDF                                                        X00553000
               POS=(11,41),                                            X00554000
               ATTRB=ASKIP,                                            X00555000
               LENGTH=01                                                00556000
         DFHMDF                                                        X00557000
               POS=(11,43),                                            X00558000
               ATTRB=ASKIP,                                            X00559000
               INITIAL=' ',                                            X00560000
               LENGTH=01                                                00561000
DTFLD56  DFHMDF                                                        X00562000
               POS=(11,46),                                            X00563000
               ATTRB=ASKIP,                                            X00564000
               LENGTH=01                                                00565000
         DFHMDF                                                        X00566000
               POS=(11,48),                                            X00567000
               ATTRB=ASKIP,                                            X00568000
               INITIAL=' ',                                            X00569000
               LENGTH=01                                                00570000
DTFLD57  DFHMDF                                                        X00571000
               POS=(11,52),                                            X00572000
               ATTRB=ASKIP,                                            X00573000
               LENGTH=01                                                00574000
         DFHMDF                                                        X00575000
               POS=(11,54),                                            X00576000
               ATTRB=ASKIP,                                            X00577000
               INITIAL=' ',                                            X00578000
               LENGTH=01                                                00579000
DTFLD58  DFHMDF                                                        X00580000
               POS=(11,58),                                            X00581000
               ATTRB=ASKIP,                                            X00582000
               LENGTH=01                                                00583000
         DFHMDF                                                        X00584000
               POS=(11,60),                                            X00585000
               ATTRB=ASKIP,                                            X00586000
               INITIAL=' ',                                            X00587000
               LENGTH=01                                                00588000
DTFLD59  DFHMDF                                                        X00589000
               POS=(11,66),                                            X00590000
               ATTRB=ASKIP,                                            X00591000
               LENGTH=01                                                00592000
         DFHMDF                                                        X00593000
               POS=(11,68),                                            X00594000
               ATTRB=ASKIP,                                            X00595000
               INITIAL=' ',                                            X00596000
               LENGTH=01                                                00597000
DESC6    DFHMDF                                                        X00598000
               POS=(12,01),                                            X00599000
               ATTRB=ASKIP,                                            X00600000
               INITIAL='TRAN CODE 06',                                 X00601000
               LENGTH=12                                                00602000
DTFLD61  DFHMDF                                                        X00603000
               POS=(12,15),                                            X00604000
               ATTRB=ASKIP,                                            X00605000
               JUSTIFY=LEFT,                                           X00606000
               LENGTH=06                                                00607000
         DFHMDF                                                        X00608000
               POS=(12,22),                                            X00609000
               ATTRB=ASKIP,                                            X00610000
               INITIAL=' ',                                            X00611000
               LENGTH=01                                                00612000
DTFLD62  DFHMDF                                                        X00613000
               POS=(12,25),                                            X00614000
               ATTRB=ASKIP,                                            X00615000
               LENGTH=01                                                00616000
         DFHMDF                                                        X00617000
               POS=(12,27),                                            X00618000
               ATTRB=ASKIP,                                            X00619000
               INITIAL=' ',                                            X00620000
               LENGTH=01                                                00621000
DTFLD63  DFHMDF                                                        X00622000
               POS=(12,31),                                            X00623000
               ATTRB=ASKIP,                                            X00624000
               LENGTH=01                                                00625000
         DFHMDF                                                        X00626000
               POS=(12,33),                                            X00627000
               ATTRB=ASKIP,                                            X00628000
               INITIAL=' ',                                            X00629000
               LENGTH=01                                                00630000
DTFLD64  DFHMDF                                                        X00631000
               POS=(12,36),                                            X00632000
               ATTRB=ASKIP,                                            X00633000
               LENGTH=01                                                00634000
         DFHMDF                                                        X00635000
               POS=(12,38),                                            X00636000
               ATTRB=ASKIP,                                            X00637000
               INITIAL=' ',                                            X00638000
               LENGTH=01                                                00639000
DTFLD65  DFHMDF                                                        X00640000
               POS=(12,41),                                            X00641000
               ATTRB=ASKIP,                                            X00642000
               LENGTH=01                                                00643000
         DFHMDF                                                        X00644000
               POS=(12,43),                                            X00645000
               ATTRB=ASKIP,                                            X00646000
               INITIAL=' ',                                            X00647000
               LENGTH=01                                                00648000
DTFLD66  DFHMDF                                                        X00649000
               POS=(12,46),                                            X00650000
               ATTRB=ASKIP,                                            X00651000
               LENGTH=01                                                00652000
         DFHMDF                                                        X00653000
               POS=(12,48),                                            X00654000
               ATTRB=ASKIP,                                            X00655000
               INITIAL=' ',                                            X00656000
               LENGTH=01                                                00657000
DTFLD67  DFHMDF                                                        X00658000
               POS=(12,52),                                            X00659000
               ATTRB=ASKIP,                                            X00660000
               LENGTH=01                                                00661000
         DFHMDF                                                        X00662000
               POS=(12,54),                                            X00663000
               ATTRB=ASKIP,                                            X00664000
               INITIAL=' ',                                            X00665000
               LENGTH=01                                                00666000
DTFLD68  DFHMDF                                                        X00667000
               POS=(12,58),                                            X00668000
               ATTRB=ASKIP,                                            X00669000
               LENGTH=01                                                00670000
         DFHMDF                                                        X00671000
               POS=(12,60),                                            X00672000
               ATTRB=ASKIP,                                            X00673000
               INITIAL=' ',                                            X00674000
               LENGTH=01                                                00675000
DTFLD69  DFHMDF                                                        X00676000
               POS=(12,66),                                            X00677000
               ATTRB=ASKIP,                                            X00678000
               LENGTH=01                                                00679000
         DFHMDF                                                        X00680000
               POS=(12,68),                                            X00681000
               ATTRB=ASKIP,                                            X00682000
               INITIAL=' ',                                            X00683000
               LENGTH=01                                                00684000
DESC7    DFHMDF                                                        X00685000
               POS=(13,01),                                            X00686000
               ATTRB=ASKIP,                                            X00687000
               INITIAL='TRAN CODE 07',                                 X00688000
               LENGTH=12                                                00689000
DTFLD71  DFHMDF                                                        X00690000
               POS=(13,15),                                            X00691000
               ATTRB=ASKIP,                                            X00692000
               JUSTIFY=LEFT,                                           X00693000
               LENGTH=06                                                00694000
         DFHMDF                                                        X00695000
               POS=(13,22),                                            X00696000
               ATTRB=ASKIP,                                            X00697000
               INITIAL=' ',                                            X00698000
               LENGTH=01                                                00699000
DTFLD72  DFHMDF                                                        X00700000
               POS=(13,25),                                            X00701000
               ATTRB=ASKIP,                                            X00702000
               LENGTH=01                                                00703000
         DFHMDF                                                        X00704000
               POS=(13,27),                                            X00705000
               ATTRB=ASKIP,                                            X00706000
               INITIAL=' ',                                            X00707000
               LENGTH=01                                                00708000
DTFLD73  DFHMDF                                                        X00709000
               POS=(13,31),                                            X00710000
               ATTRB=ASKIP,                                            X00711000
               LENGTH=01                                                00712000
         DFHMDF                                                        X00713000
               POS=(13,33),                                            X00714000
               ATTRB=ASKIP,                                            X00715000
               INITIAL=' ',                                            X00716000
               LENGTH=01                                                00717000
DTFLD74  DFHMDF                                                        X00718000
               POS=(13,36),                                            X00719000
               ATTRB=ASKIP,                                            X00720000
               LENGTH=01                                                00721000
         DFHMDF                                                        X00722000
               POS=(13,38),                                            X00723000
               ATTRB=ASKIP,                                            X00724000
               INITIAL=' ',                                            X00725000
               LENGTH=01                                                00726000
DTFLD75  DFHMDF                                                        X00727000
               POS=(13,41),                                            X00728000
               ATTRB=ASKIP,                                            X00729000
               LENGTH=01                                                00730000
         DFHMDF                                                        X00731000
               POS=(13,43),                                            X00732000
               ATTRB=ASKIP,                                            X00733000
               INITIAL=' ',                                            X00734000
               LENGTH=01                                                00735000
DTFLD76  DFHMDF                                                        X00736000
               POS=(13,46),                                            X00737000
               ATTRB=ASKIP,                                            X00738000
               LENGTH=01                                                00739000
         DFHMDF                                                        X00740000
               POS=(13,48),                                            X00741000
               ATTRB=ASKIP,                                            X00742000
               INITIAL=' ',                                            X00743000
               LENGTH=01                                                00744000
DTFLD77  DFHMDF                                                        X00745000
               POS=(13,52),                                            X00746000
               ATTRB=ASKIP,                                            X00747000
               LENGTH=01                                                00748000
         DFHMDF                                                        X00749000
               POS=(13,54),                                            X00750000
               ATTRB=ASKIP,                                            X00751000
               INITIAL=' ',                                            X00752000
               LENGTH=01                                                00753000
DTFLD78  DFHMDF                                                        X00754000
               POS=(13,58),                                            X00755000
               ATTRB=ASKIP,                                            X00756000
               LENGTH=01                                                00757000
         DFHMDF                                                        X00758000
               POS=(13,60),                                            X00759000
               ATTRB=ASKIP,                                            X00760000
               INITIAL=' ',                                            X00761000
               LENGTH=01                                                00762000
DTFLD79  DFHMDF                                                        X00763000
               POS=(13,66),                                            X00764000
               ATTRB=ASKIP,                                            X00765000
               LENGTH=01                                                00766000
         DFHMDF                                                        X00767000
               POS=(13,68),                                            X00768000
               ATTRB=ASKIP,                                            X00769000
               INITIAL=' ',                                            X00770000
               LENGTH=01                                                00771000
DESC8    DFHMDF                                                        X00772000
               POS=(14,01),                                            X00773000
               ATTRB=ASKIP,                                            X00774000
               INITIAL='TRAN CODE 08',                                 X00775000
               LENGTH=12                                                00776000
DTFLD81  DFHMDF                                                        X00777000
               POS=(14,15),                                            X00778000
               ATTRB=ASKIP,                                            X00779000
               JUSTIFY=LEFT,                                           X00780000
               LENGTH=06                                                00781000
         DFHMDF                                                        X00782000
               POS=(14,22),                                            X00783000
               ATTRB=ASKIP,                                            X00784000
               INITIAL=' ',                                            X00785000
               LENGTH=01                                                00786000
DTFLD82  DFHMDF                                                        X00787000
               POS=(14,25),                                            X00788000
               ATTRB=ASKIP,                                            X00789000
               LENGTH=01                                                00790000
         DFHMDF                                                        X00791000
               POS=(14,27),                                            X00792000
               ATTRB=ASKIP,                                            X00793000
               INITIAL=' ',                                            X00794000
               LENGTH=01                                                00795000
DTFLD83  DFHMDF                                                        X00796000
               POS=(14,31),                                            X00797000
               ATTRB=ASKIP,                                            X00798000
               LENGTH=01                                                00799000
         DFHMDF                                                        X00800000
               POS=(14,33),                                            X00801000
               ATTRB=ASKIP,                                            X00802000
               INITIAL=' ',                                            X00803000
               LENGTH=01                                                00804000
DTFLD84  DFHMDF                                                        X00805000
               POS=(14,36),                                            X00806000
               ATTRB=ASKIP,                                            X00807000
               LENGTH=01                                                00808000
         DFHMDF                                                        X00809000
               POS=(14,38),                                            X00810000
               ATTRB=ASKIP,                                            X00811000
               INITIAL=' ',                                            X00812000
               LENGTH=01                                                00813000
DTFLD85  DFHMDF                                                        X00814000
               POS=(14,41),                                            X00815000
               ATTRB=ASKIP,                                            X00816000
               LENGTH=01                                                00817000
         DFHMDF                                                        X00818000
               POS=(14,43),                                            X00819000
               ATTRB=ASKIP,                                            X00820000
               INITIAL=' ',                                            X00821000
               LENGTH=01                                                00822000
DTFLD86  DFHMDF                                                        X00823000
               POS=(14,46),                                            X00824000
               ATTRB=ASKIP,                                            X00825000
               LENGTH=01                                                00826000
         DFHMDF                                                        X00827000
               POS=(14,48),                                            X00828000
               ATTRB=ASKIP,                                            X00829000
               INITIAL=' ',                                            X00830000
               LENGTH=01                                                00831000
DTFLD87  DFHMDF                                                        X00832000
               POS=(14,52),                                            X00833000
               ATTRB=ASKIP,                                            X00834000
               LENGTH=01                                                00835000
         DFHMDF                                                        X00836000
               POS=(14,54),                                            X00837000
               ATTRB=ASKIP,                                            X00838000
               INITIAL=' ',                                            X00839000
               LENGTH=01                                                00840000
DTFLD88  DFHMDF                                                        X00841000
               POS=(14,58),                                            X00842000
               ATTRB=ASKIP,                                            X00843000
               LENGTH=01                                                00844000
         DFHMDF                                                        X00845000
               POS=(14,60),                                            X00846000
               ATTRB=ASKIP,                                            X00847000
               INITIAL=' ',                                            X00848000
               LENGTH=01                                                00849000
DTFLD89  DFHMDF                                                        X00850000
               POS=(14,66),                                            X00851000
               ATTRB=ASKIP,                                            X00852000
               LENGTH=01                                                00853000
         DFHMDF                                                        X00854000
               POS=(14,68),                                            X00855000
               ATTRB=ASKIP,                                            X00856000
               INITIAL=' ',                                            X00857000
               LENGTH=01                                                00858000
DESC9    DFHMDF                                                        X00859000
               POS=(15,01),                                            X00860000
               ATTRB=ASKIP,                                            X00861000
               INITIAL='TRAN CODE 09',                                 X00862000
               LENGTH=12                                                00863000
DTFLD91  DFHMDF                                                        X00864000
               POS=(15,15),                                            X00865000
               ATTRB=ASKIP,                                            X00866000
               JUSTIFY=LEFT,                                           X00867000
               LENGTH=06                                                00868000
         DFHMDF                                                        X00869000
               POS=(15,22),                                            X00870000
               ATTRB=ASKIP,                                            X00871000
               INITIAL=' ',                                            X00872000
               LENGTH=01                                                00873000
DTFLD92  DFHMDF                                                        X00874000
               POS=(15,25),                                            X00875000
               ATTRB=ASKIP,                                            X00876000
               LENGTH=01                                                00877000
         DFHMDF                                                        X00878000
               POS=(15,27),                                            X00879000
               ATTRB=ASKIP,                                            X00880000
               INITIAL=' ',                                            X00881000
               LENGTH=01                                                00882000
DTFLD93  DFHMDF                                                        X00883000
               POS=(15,31),                                            X00884000
               ATTRB=ASKIP,                                            X00885000
               LENGTH=01                                                00886000
         DFHMDF                                                        X00887000
               POS=(15,33),                                            X00888000
               ATTRB=ASKIP,                                            X00889000
               INITIAL=' ',                                            X00890000
               LENGTH=01                                                00891000
DTFLD94  DFHMDF                                                        X00892000
               POS=(15,36),                                            X00893000
               ATTRB=ASKIP,                                            X00894000
               LENGTH=01                                                00895000
         DFHMDF                                                        X00896000
               POS=(15,38),                                            X00897000
               ATTRB=ASKIP,                                            X00898000
               INITIAL=' ',                                            X00899000
               LENGTH=01                                                00900000
DTFLD95  DFHMDF                                                        X00901000
               POS=(15,41),                                            X00902000
               ATTRB=ASKIP,                                            X00903000
               LENGTH=01                                                00904000
         DFHMDF                                                        X00905000
               POS=(15,43),                                            X00906000
               ATTRB=ASKIP,                                            X00907000
               INITIAL=' ',                                            X00908000
               LENGTH=01                                                00909000
DTFLD96  DFHMDF                                                        X00910000
               POS=(15,46),                                            X00911000
               ATTRB=ASKIP,                                            X00912000
               LENGTH=01                                                00913000
         DFHMDF                                                        X00914000
               POS=(15,48),                                            X00915000
               ATTRB=ASKIP,                                            X00916000
               INITIAL=' ',                                            X00917000
               LENGTH=01                                                00918000
DTFLD97  DFHMDF                                                        X00919000
               POS=(15,52),                                            X00920000
               ATTRB=ASKIP,                                            X00921000
               LENGTH=01                                                00922000
         DFHMDF                                                        X00923000
               POS=(15,54),                                            X00924000
               ATTRB=ASKIP,                                            X00925000
               INITIAL=' ',                                            X00926000
               LENGTH=01                                                00927000
DTFLD98  DFHMDF                                                        X00928000
               POS=(15,58),                                            X00929000
               ATTRB=ASKIP,                                            X00930000
               LENGTH=01                                                00931000
         DFHMDF                                                        X00932000
               POS=(15,60),                                            X00933000
               ATTRB=ASKIP,                                            X00934000
               INITIAL=' ',                                            X00935000
               LENGTH=01                                                00936000
DTFLD99  DFHMDF                                                        X00937000
               POS=(15,66),                                            X00938000
               ATTRB=ASKIP,                                            X00939000
               LENGTH=01                                                00940000
         DFHMDF                                                        X00941000
               POS=(15,68),                                            X00942000
               ATTRB=ASKIP,                                            X00943000
               INITIAL=' ',                                            X00944000
               LENGTH=01                                                00945000
DESC10   DFHMDF                                                        X00946000
               POS=(16,01),                                            X00947000
               ATTRB=ASKIP,                                            X00948000
               INITIAL='TRAN CODE 10',                                 X00949000
               LENGTH=12                                                00950000
DTFLDA1  DFHMDF                                                        X00951000
               POS=(16,15),                                            X00952000
               ATTRB=ASKIP,                                            X00953000
               JUSTIFY=LEFT,                                           X00954000
               LENGTH=06                                                00955000
         DFHMDF                                                        X00956000
               POS=(16,22),                                            X00957000
               ATTRB=ASKIP,                                            X00958000
               INITIAL=' ',                                            X00959000
               LENGTH=01                                                00960000
DTFLDA2  DFHMDF                                                        X00961000
               POS=(16,25),                                            X00962000
               ATTRB=ASKIP,                                            X00963000
               LENGTH=01                                                00964000
         DFHMDF                                                        X00965000
               POS=(16,27),                                            X00966000
               ATTRB=ASKIP,                                            X00967000
               INITIAL=' ',                                            X00968000
               LENGTH=01                                                00969000
DTFLDA3  DFHMDF                                                        X00970000
               POS=(16,31),                                            X00971000
               ATTRB=ASKIP,                                            X00972000
               LENGTH=01                                                00973000
         DFHMDF                                                        X00974000
               POS=(16,33),                                            X00975000
               ATTRB=ASKIP,                                            X00976000
               INITIAL=' ',                                            X00977000
               LENGTH=01                                                00978000
DTFLDA4  DFHMDF                                                        X00979000
               POS=(16,36),                                            X00980000
               ATTRB=ASKIP,                                            X00981000
               LENGTH=01                                                00982000
         DFHMDF                                                        X00983000
               POS=(16,38),                                            X00984000
               ATTRB=ASKIP,                                            X00985000
               INITIAL=' ',                                            X00986000
               LENGTH=01                                                00987000
DTFLDA5  DFHMDF                                                        X00988000
               POS=(16,41),                                            X00989000
               ATTRB=ASKIP,                                            X00990000
               LENGTH=01                                                00991000
         DFHMDF                                                        X00992000
               POS=(16,43),                                            X00993000
               ATTRB=ASKIP,                                            X00994000
               INITIAL=' ',                                            X00995000
               LENGTH=01                                                00996000
DTFLDA6  DFHMDF                                                        X00997000
               POS=(16,46),                                            X00998000
               ATTRB=ASKIP,                                            X00999000
               LENGTH=01                                                01000000
         DFHMDF                                                        X01001000
               POS=(16,48),                                            X01002000
               ATTRB=ASKIP,                                            X01003000
               INITIAL=' ',                                            X01004000
               LENGTH=01                                                01005000
DTFLDA7  DFHMDF                                                        X01006000
               POS=(16,52),                                            X01007000
               ATTRB=ASKIP,                                            X01008000
               LENGTH=01                                                01009000
         DFHMDF                                                        X01010000
               POS=(16,54),                                            X01011000
               ATTRB=ASKIP,                                            X01012000
               INITIAL=' ',                                            X01013000
               LENGTH=01                                                01014000
DTFLDA8  DFHMDF                                                        X01015000
               POS=(16,58),                                            X01016000
               ATTRB=ASKIP,                                            X01017000
               LENGTH=01                                                01018000
         DFHMDF                                                        X01019000
               POS=(16,60),                                            X01020000
               ATTRB=ASKIP,                                            X01021000
               INITIAL=' ',                                            X01022000
               LENGTH=01                                                01023000
DTFLDA9  DFHMDF                                                        X01024000
               POS=(16,66),                                            X01025000
               ATTRB=ASKIP,                                            X01026000
               LENGTH=01                                                01027000
         DFHMDF                                                        X01028000
               POS=(16,68),                                            X01029000
               ATTRB=ASKIP,                                            X01030000
               INITIAL=' ',                                            X01031000
               LENGTH=01                                                01032000
DESC11   DFHMDF                                                        X01033000
               POS=(17,01),                                            X01034000
               ATTRB=ASKIP,                                            X01035000
               INITIAL='TRAN CODE 11',                                 X01036000
               LENGTH=12                                                01037000
DTFLDB1  DFHMDF                                                        X01038000
               POS=(17,15),                                            X01039000
               ATTRB=ASKIP,                                            X01040000
               JUSTIFY=LEFT,                                           X01041000
               LENGTH=06                                                01042000
         DFHMDF                                                        X01043000
               POS=(17,22),                                            X01044000
               ATTRB=ASKIP,                                            X01045000
               INITIAL=' ',                                            X01046000
               LENGTH=01                                                01047000
DTFLDB2  DFHMDF                                                        X01048000
               POS=(17,25),                                            X01049000
               ATTRB=ASKIP,                                            X01050000
               LENGTH=01                                                01051000
         DFHMDF                                                        X01052000
               POS=(17,27),                                            X01053000
               ATTRB=ASKIP,                                            X01054000
               INITIAL=' ',                                            X01055000
               LENGTH=01                                                01056000
DTFLDB3  DFHMDF                                                        X01057000
               POS=(17,31),                                            X01058000
               ATTRB=ASKIP,                                            X01059000
               LENGTH=01                                                01060000
         DFHMDF                                                        X01061000
               POS=(17,33),                                            X01062000
               ATTRB=ASKIP,                                            X01063000
               INITIAL=' ',                                            X01064000
               LENGTH=01                                                01065000
DTFLDB4  DFHMDF                                                        X01066000
               POS=(17,36),                                            X01067000
               ATTRB=ASKIP,                                            X01068000
               LENGTH=01                                                01069000
         DFHMDF                                                        X01070000
               POS=(17,38),                                            X01071000
               ATTRB=ASKIP,                                            X01072000
               INITIAL=' ',                                            X01073000
               LENGTH=01                                                01074000
DTFLDB5  DFHMDF                                                        X01075000
               POS=(17,41),                                            X01076000
               ATTRB=ASKIP,                                            X01077000
               LENGTH=01                                                01078000
         DFHMDF                                                        X01079000
               POS=(17,43),                                            X01080000
               ATTRB=ASKIP,                                            X01081000
               INITIAL=' ',                                            X01082000
               LENGTH=01                                                01083000
DTFLDB6  DFHMDF                                                        X01084000
               POS=(17,46),                                            X01085000
               ATTRB=ASKIP,                                            X01086000
               LENGTH=01                                                01087000
         DFHMDF                                                        X01088000
               POS=(17,48),                                            X01089000
               ATTRB=ASKIP,                                            X01090000
               INITIAL=' ',                                            X01091000
               LENGTH=01                                                01092000
DTFLDB7  DFHMDF                                                        X01093000
               POS=(17,52),                                            X01094000
               ATTRB=ASKIP,                                            X01095000
               LENGTH=01                                                01096000
         DFHMDF                                                        X01097000
               POS=(17,54),                                            X01098000
               ATTRB=ASKIP,                                            X01099000
               INITIAL=' ',                                            X01100000
               LENGTH=01                                                01101000
DTFLDB8  DFHMDF                                                        X01102000
               POS=(17,58),                                            X01103000
               ATTRB=ASKIP,                                            X01104000
               LENGTH=01                                                01105000
         DFHMDF                                                        X01106000
               POS=(17,60),                                            X01107000
               ATTRB=ASKIP,                                            X01108000
               INITIAL=' ',                                            X01109000
               LENGTH=01                                                01110000
DTFLDB9  DFHMDF                                                        X01111000
               POS=(17,66),                                            X01112000
               ATTRB=ASKIP,                                            X01113000
               LENGTH=01                                                01114000
         DFHMDF                                                        X01115000
               POS=(17,68),                                            X01116000
               ATTRB=ASKIP,                                            X01117000
               INITIAL=' ',                                            X01118000
               LENGTH=01                                                01119000
DESC12   DFHMDF                                                        X01120000
               POS=(18,01),                                            X01121000
               ATTRB=ASKIP,                                            X01122000
               INITIAL='TRAN CODE 12',                                 X01123000
               LENGTH=12                                                01124000
DTFLDC1  DFHMDF                                                        X01125000
               POS=(18,15),                                            X01126000
               ATTRB=ASKIP,                                            X01127000
               JUSTIFY=LEFT,                                           X01128000
               LENGTH=06                                                01129000
         DFHMDF                                                        X01130000
               POS=(18,22),                                            X01131000
               ATTRB=ASKIP,                                            X01132000
               INITIAL=' ',                                            X01133000
               LENGTH=01                                                01134000
DTFLDC2  DFHMDF                                                        X01135000
               POS=(18,25),                                            X01136000
               ATTRB=ASKIP,                                            X01137000
               LENGTH=01                                                01138000
         DFHMDF                                                        X01139000
               POS=(18,27),                                            X01140000
               ATTRB=ASKIP,                                            X01141000
               INITIAL=' ',                                            X01142000
               LENGTH=01                                                01143000
DTFLDC3  DFHMDF                                                        X01144000
               POS=(18,31),                                            X01145000
               ATTRB=ASKIP,                                            X01146000
               LENGTH=01                                                01147000
         DFHMDF                                                        X01148000
               POS=(18,33),                                            X01149000
               ATTRB=ASKIP,                                            X01150000
               INITIAL=' ',                                            X01151000
               LENGTH=01                                                01152000
DTFLDC4  DFHMDF                                                        X01153000
               POS=(18,36),                                            X01154000
               ATTRB=ASKIP,                                            X01155000
               LENGTH=01                                                01156000
         DFHMDF                                                        X01157000
               POS=(18,38),                                            X01158000
               ATTRB=ASKIP,                                            X01159000
               INITIAL=' ',                                            X01160000
               LENGTH=01                                                01161000
DTFLDC5  DFHMDF                                                        X01162000
               POS=(18,41),                                            X01163000
               ATTRB=ASKIP,                                            X01164000
               LENGTH=01                                                01165000
         DFHMDF                                                        X01166000
               POS=(18,43),                                            X01167000
               ATTRB=ASKIP,                                            X01168000
               INITIAL=' ',                                            X01169000
               LENGTH=01                                                01170000
DTFLDC6  DFHMDF                                                        X01171000
               POS=(18,46),                                            X01172000
               ATTRB=ASKIP,                                            X01173000
               LENGTH=01                                                01174000
         DFHMDF                                                        X01175000
               POS=(18,48),                                            X01176000
               ATTRB=ASKIP,                                            X01177000
               INITIAL=' ',                                            X01178000
               LENGTH=01                                                01179000
DTFLDC7  DFHMDF                                                        X01180000
               POS=(18,52),                                            X01181000
               ATTRB=ASKIP,                                            X01182000
               LENGTH=01                                                01183000
         DFHMDF                                                        X01184000
               POS=(18,54),                                            X01185000
               ATTRB=ASKIP,                                            X01186000
               INITIAL=' ',                                            X01187000
               LENGTH=01                                                01188000
DTFLDC8  DFHMDF                                                        X01189000
               POS=(18,58),                                            X01190000
               ATTRB=ASKIP,                                            X01191000
               LENGTH=01                                                01192000
         DFHMDF                                                        X01193000
               POS=(18,60),                                            X01194000
               ATTRB=ASKIP,                                            X01195000
               INITIAL=' ',                                            X01196000
               LENGTH=01                                                01197000
DTFLDC9  DFHMDF                                                        X01198000
               POS=(18,66),                                            X01199000
               ATTRB=ASKIP,                                            X01200000
               LENGTH=01                                                01201000
         DFHMDF                                                        X01202000
               POS=(18,68),                                            X01203000
               ATTRB=ASKIP,                                            X01204000
               INITIAL=' ',                                            X01205000
               LENGTH=01                                                01206000
DESC13   DFHMDF                                                        X01207000
               POS=(19,01),                                            X01208000
               ATTRB=ASKIP,                                            X01209000
               INITIAL='TRAN CODE 13',                                 X01210000
               LENGTH=12                                                01211000
DTFLDD1  DFHMDF                                                        X01212000
               POS=(19,15),                                            X01213000
               ATTRB=ASKIP,                                            X01214000
               JUSTIFY=LEFT,                                           X01215000
               LENGTH=06                                                01216000
         DFHMDF                                                        X01217000
               POS=(19,22),                                            X01218000
               ATTRB=ASKIP,                                            X01219000
               INITIAL=' ',                                            X01220000
               LENGTH=01                                                01221000
DTFLDD2  DFHMDF                                                        X01222000
               POS=(19,25),                                            X01223000
               ATTRB=ASKIP,                                            X01224000
               LENGTH=01                                                01225000
         DFHMDF                                                        X01226000
               POS=(19,27),                                            X01227000
               ATTRB=ASKIP,                                            X01228000
               INITIAL=' ',                                            X01229000
               LENGTH=01                                                01230000
DTFLDD3  DFHMDF                                                        X01231000
               POS=(19,31),                                            X01232000
               ATTRB=ASKIP,                                            X01233000
               LENGTH=01                                                01234000
         DFHMDF                                                        X01235000
               POS=(19,33),                                            X01236000
               ATTRB=ASKIP,                                            X01237000
               INITIAL=' ',                                            X01238000
               LENGTH=01                                                01239000
DTFLDD4  DFHMDF                                                        X01240000
               POS=(19,36),                                            X01241000
               ATTRB=ASKIP,                                            X01242000
               LENGTH=01                                                01243000
         DFHMDF                                                        X01244000
               POS=(19,38),                                            X01245000
               ATTRB=ASKIP,                                            X01246000
               INITIAL=' ',                                            X01247000
               LENGTH=01                                                01248000
DTFLDD5  DFHMDF                                                        X01249000
               POS=(19,41),                                            X01250000
               ATTRB=ASKIP,                                            X01251000
               LENGTH=01                                                01252000
         DFHMDF                                                        X01253000
               POS=(19,43),                                            X01254000
               ATTRB=ASKIP,                                            X01255000
               INITIAL=' ',                                            X01256000
               LENGTH=01                                                01257000
DTFLDD6  DFHMDF                                                        X01258000
               POS=(19,46),                                            X01259000
               ATTRB=ASKIP,                                            X01260000
               LENGTH=01                                                01261000
         DFHMDF                                                        X01262000
               POS=(19,48),                                            X01263000
               ATTRB=ASKIP,                                            X01264000
               INITIAL=' ',                                            X01265000
               LENGTH=01                                                01266000
DTFLDD7  DFHMDF                                                        X01267000
               POS=(19,52),                                            X01268000
               ATTRB=ASKIP,                                            X01269000
               LENGTH=01                                                01270000
         DFHMDF                                                        X01271000
               POS=(19,54),                                            X01272000
               ATTRB=ASKIP,                                            X01273000
               INITIAL=' ',                                            X01274000
               LENGTH=01                                                01275000
DTFLDD8  DFHMDF                                                        X01276000
               POS=(19,58),                                            X01277000
               ATTRB=ASKIP,                                            X01278000
               LENGTH=01                                                01279000
         DFHMDF                                                        X01280000
               POS=(19,60),                                            X01281000
               ATTRB=ASKIP,                                            X01282000
               INITIAL=' ',                                            X01283000
               LENGTH=01                                                01284000
DTFLDD9  DFHMDF                                                        X01285000
               POS=(19,66),                                            X01286000
               ATTRB=ASKIP,                                            X01287000
               LENGTH=01                                                01288000
         DFHMDF                                                        X01289000
               POS=(19,68),                                            X01290000
               ATTRB=ASKIP,                                            X01291000
               INITIAL=' ',                                            X01292000
               LENGTH=01                                                01293000
DESC14   DFHMDF                                                        X01294000
               POS=(20,01),                                            X01295000
               ATTRB=ASKIP,                                            X01296000
               INITIAL='TRAN CODE 14',                                 X01297000
               LENGTH=12                                                01298000
DTFLDE1  DFHMDF                                                        X01299000
               POS=(20,15),                                            X01300000
               ATTRB=ASKIP,                                            X01301000
               JUSTIFY=LEFT,                                           X01302000
               LENGTH=06                                                01303000
         DFHMDF                                                        X01304000
               POS=(20,22),                                            X01305000
               ATTRB=ASKIP,                                            X01306000
               INITIAL=' ',                                            X01307000
               LENGTH=01                                                01308000
DTFLDE2  DFHMDF                                                        X01309000
               POS=(20,25),                                            X01310000
               ATTRB=ASKIP,                                            X01311000
               LENGTH=01                                                01312000
         DFHMDF                                                        X01313000
               POS=(20,27),                                            X01314000
               ATTRB=ASKIP,                                            X01315000
               INITIAL=' ',                                            X01316000
               LENGTH=01                                                01317000
DTFLDE3  DFHMDF                                                        X01318000
               POS=(20,31),                                            X01319000
               ATTRB=ASKIP,                                            X01320000
               LENGTH=01                                                01321000
         DFHMDF                                                        X01322000
               POS=(20,33),                                            X01323000
               ATTRB=ASKIP,                                            X01324000
               INITIAL=' ',                                            X01325000
               LENGTH=01                                                01326000
DTFLDE4  DFHMDF                                                        X01327000
               POS=(20,36),                                            X01328000
               ATTRB=ASKIP,                                            X01329000
               LENGTH=01                                                01330000
         DFHMDF                                                        X01331000
               POS=(20,38),                                            X01332000
               ATTRB=ASKIP,                                            X01333000
               INITIAL=' ',                                            X01334000
               LENGTH=01                                                01335000
DTFLDE5  DFHMDF                                                        X01336000
               POS=(20,41),                                            X01337000
               ATTRB=ASKIP,                                            X01338000
               LENGTH=01                                                01339000
         DFHMDF                                                        X01340000
               POS=(20,43),                                            X01341000
               ATTRB=ASKIP,                                            X01342000
               INITIAL=' ',                                            X01343000
               LENGTH=01                                                01344000
DTFLDE6  DFHMDF                                                        X01345000
               POS=(20,46),                                            X01346000
               ATTRB=ASKIP,                                            X01347000
               LENGTH=01                                                01348000
         DFHMDF                                                        X01349000
               POS=(20,48),                                            X01350000
               ATTRB=ASKIP,                                            X01351000
               INITIAL=' ',                                            X01352000
               LENGTH=01                                                01353000
DTFLDE7  DFHMDF                                                        X01354000
               POS=(20,52),                                            X01355000
               ATTRB=ASKIP,                                            X01356000
               LENGTH=01                                                01357000
         DFHMDF                                                        X01358000
               POS=(20,54),                                            X01359000
               ATTRB=ASKIP,                                            X01360000
               INITIAL=' ',                                            X01361000
               LENGTH=01                                                01362000
DTFLDE8  DFHMDF                                                        X01363000
               POS=(20,58),                                            X01364000
               ATTRB=ASKIP,                                            X01365000
               LENGTH=01                                                01366000
         DFHMDF                                                        X01367000
               POS=(20,60),                                            X01368000
               ATTRB=ASKIP,                                            X01369000
               INITIAL=' ',                                            X01370000
               LENGTH=01                                                01371000
DTFLDE9  DFHMDF                                                        X01372000
               POS=(20,66),                                            X01373000
               ATTRB=ASKIP,                                            X01374000
               LENGTH=01                                                01375000
         DFHMDF                                                        X01376000
               POS=(20,68),                                            X01377000
               ATTRB=ASKIP,                                            X01378000
               INITIAL=' ',                                            X01379000
               LENGTH=01                                                01380000
DESC15   DFHMDF                                                        X01381000
               POS=(21,01),                                            X01382000
               ATTRB=ASKIP,                                            X01383000
               INITIAL='TRAN CODE 15',                                 X01384000
               LENGTH=12                                                01385000
DTFLDF1  DFHMDF                                                        X01386000
               POS=(21,15),                                            X01387000
               ATTRB=ASKIP,                                            X01388000
               JUSTIFY=LEFT,                                           X01389000
               LENGTH=06                                                01390000
         DFHMDF                                                        X01391000
               POS=(21,22),                                            X01392000
               ATTRB=ASKIP,                                            X01393000
               INITIAL=' ',                                            X01394000
               LENGTH=01                                                01395000
DTFLDF2  DFHMDF                                                        X01396000
               POS=(21,25),                                            X01397000
               ATTRB=ASKIP,                                            X01398000
               LENGTH=01                                                01399000
         DFHMDF                                                        X01400000
               POS=(21,27),                                            X01401000
               ATTRB=ASKIP,                                            X01402000
               INITIAL=' ',                                            X01403000
               LENGTH=01                                                01404000
DTFLDF3  DFHMDF                                                        X01405000
               POS=(21,31),                                            X01406000
               ATTRB=ASKIP,                                            X01407000
               LENGTH=01                                                01408000
         DFHMDF                                                        X01409000
               POS=(21,33),                                            X01410000
               ATTRB=ASKIP,                                            X01411000
               INITIAL=' ',                                            X01412000
               LENGTH=01                                                01413000
DTFLDF4  DFHMDF                                                        X01414000
               POS=(21,36),                                            X01415000
               ATTRB=ASKIP,                                            X01416000
               LENGTH=01                                                01417000
         DFHMDF                                                        X01418000
               POS=(21,38),                                            X01419000
               ATTRB=ASKIP,                                            X01420000
               INITIAL=' ',                                            X01421000
               LENGTH=01                                                01422000
DTFLDF5  DFHMDF                                                        X01423000
               POS=(21,41),                                            X01424000
               ATTRB=ASKIP,                                            X01425000
               LENGTH=01                                                01426000
         DFHMDF                                                        X01427000
               POS=(21,43),                                            X01428000
               ATTRB=ASKIP,                                            X01429000
               INITIAL=' ',                                            X01430000
               LENGTH=01                                                01431000
DTFLDF6  DFHMDF                                                        X01432000
               POS=(21,46),                                            X01433000
               ATTRB=ASKIP,                                            X01434000
               LENGTH=01                                                01435000
         DFHMDF                                                        X01436000
               POS=(21,48),                                            X01437000
               ATTRB=ASKIP,                                            X01438000
               INITIAL=' ',                                            X01439000
               LENGTH=01                                                01440000
DTFLDF7  DFHMDF                                                        X01441000
               POS=(21,52),                                            X01442000
               ATTRB=ASKIP,                                            X01443000
               LENGTH=01                                                01444000
         DFHMDF                                                        X01445000
               POS=(21,54),                                            X01446000
               ATTRB=ASKIP,                                            X01447000
               INITIAL=' ',                                            X01448000
               LENGTH=01                                                01449000
DTFLDF8  DFHMDF                                                        X01450000
               POS=(21,58),                                            X01451000
               ATTRB=ASKIP,                                            X01452000
               LENGTH=01                                                01453000
         DFHMDF                                                        X01454000
               POS=(21,60),                                            X01455000
               ATTRB=ASKIP,                                            X01456000
               INITIAL=' ',                                            X01457000
               LENGTH=01                                                01458000
DTFLDF9  DFHMDF                                                        X01459000
               POS=(21,66),                                            X01460000
               ATTRB=ASKIP,                                            X01461000
               LENGTH=01                                                01462000
         DFHMDF                                                        X01463000
               POS=(21,68),                                            X01464000
               ATTRB=ASKIP,                                            X01465000
               INITIAL=' ',                                            X01466000
               LENGTH=01                                                01467000
DESC16   DFHMDF                                                        X01468000
               POS=(22,01),                                            X01469000
               ATTRB=ASKIP,                                            X01470000
               INITIAL='TRAN CODE 16',                                 X01471000
               LENGTH=12                                                01472000
DTFLDG1  DFHMDF                                                        X01473000
               POS=(22,15),                                            X01474000
               ATTRB=ASKIP,                                            X01475000
               JUSTIFY=LEFT,                                           X01476000
               LENGTH=06                                                01477000
         DFHMDF                                                        X01478000
               POS=(22,22),                                            X01479000
               ATTRB=ASKIP,                                            X01480000
               INITIAL=' ',                                            X01481000
               LENGTH=01                                                01482000
DTFLDG2  DFHMDF                                                        X01483000
               POS=(22,25),                                            X01484000
               ATTRB=ASKIP,                                            X01485000
               LENGTH=01                                                01486000
         DFHMDF                                                        X01487000
               POS=(22,27),                                            X01488000
               ATTRB=ASKIP,                                            X01489000
               INITIAL=' ',                                            X01490000
               LENGTH=01                                                01491000
DTFLDG3  DFHMDF                                                        X01492000
               POS=(22,31),                                            X01493000
               ATTRB=ASKIP,                                            X01494000
               LENGTH=01                                                01495000
         DFHMDF                                                        X01496000
               POS=(22,33),                                            X01497000
               ATTRB=ASKIP,                                            X01498000
               INITIAL=' ',                                            X01499000
               LENGTH=01                                                01500000
DTFLDG4  DFHMDF                                                        X01501000
               POS=(22,36),                                            X01502000
               ATTRB=ASKIP,                                            X01503000
               LENGTH=01                                                01504000
         DFHMDF                                                        X01505000
               POS=(22,38),                                            X01506000
               ATTRB=ASKIP,                                            X01507000
               INITIAL=' ',                                            X01508000
               LENGTH=01                                                01509000
DTFLDG5  DFHMDF                                                        X01510000
               POS=(22,41),                                            X01511000
               ATTRB=ASKIP,                                            X01512000
               LENGTH=01                                                01513000
         DFHMDF                                                        X01514000
               POS=(22,43),                                            X01515000
               ATTRB=ASKIP,                                            X01516000
               INITIAL=' ',                                            X01517000
               LENGTH=01                                                01518000
DTFLDG6  DFHMDF                                                        X01519000
               POS=(22,46),                                            X01520000
               ATTRB=ASKIP,                                            X01521000
               LENGTH=01                                                01522000
         DFHMDF                                                        X01523000
               POS=(22,48),                                            X01524000
               ATTRB=ASKIP,                                            X01525000
               INITIAL=' ',                                            X01526000
               LENGTH=01                                                01527000
DTFLDG7  DFHMDF                                                        X01528000
               POS=(22,52),                                            X01529000
               ATTRB=ASKIP,                                            X01530000
               LENGTH=01                                                01531000
         DFHMDF                                                        X01532000
               POS=(22,54),                                            X01533000
               ATTRB=ASKIP,                                            X01534000
               INITIAL=' ',                                            X01535000
               LENGTH=01                                                01536000
DTFLDG8  DFHMDF                                                        X01537000
               POS=(22,58),                                            X01538000
               ATTRB=ASKIP,                                            X01539000
               LENGTH=01                                                01540000
         DFHMDF                                                        X01541000
               POS=(22,60),                                            X01542000
               ATTRB=ASKIP,                                            X01543000
               INITIAL=' ',                                            X01544000
               LENGTH=01                                                01545000
DTFLDG9  DFHMDF                                                        X01546000
               POS=(22,66),                                            X01547000
               ATTRB=ASKIP,                                            X01548000
               LENGTH=01                                                01549000
         DFHMDF                                                        X01550000
               POS=(22,68),                                            X01551000
               ATTRB=ASKIP,                                            X01552000
               INITIAL=' ',                                            X01553000
               LENGTH=01                                                01554000
         COPY  MIN920                                                   01555000
