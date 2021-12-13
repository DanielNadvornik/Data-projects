* Encoding: UTF-8.
*DATA ESS.
CROSSTABS gndr by lrscale.

*DATA CVVM.
CROSSTABS PO_2 by IDE_8.
MISSING VALUES PO_2 (97,99).

IF(IDE_8 = 1 AND PO_2 = 1) scaleXsex = 1.
IF(IDE_8 = 1 AND PO_2 = 2) scaleXsex = 2.
IF(IDE_8 = 1 AND PO_2 = 3) scaleXsex = 3.
IF(IDE_8 = 1 AND PO_2 = 4) scaleXsex = 4.
IF(IDE_8 = 1 AND PO_2 = 5) scaleXsex = 5.
IF(IDE_8 = 1 AND PO_2 = 6) scaleXsex = 6.
IF(IDE_8 = 1 AND PO_2 = 7) scaleXsex = 7.
IF(IDE_8 = 1 AND PO_2 = 8) scaleXsex = 8.
IF(IDE_8 = 1 AND PO_2 = 9) scaleXsex = 9.
IF(IDE_8 = 1 AND PO_2 = 10) scaleXsex = 10.
IF(IDE_8 = 1 AND PO_2 = 11) scaleXsex = 11.
IF(IDE_8 = 2 AND PO_2 = 1) scaleXsex = 12.
IF(IDE_8 = 2 AND PO_2 = 2) scaleXsex = 13.
IF(IDE_8 = 2 AND PO_2 = 3) scaleXsex = 14.
IF(IDE_8 = 2 AND PO_2 = 4) scaleXsex = 15.
IF(IDE_8 = 2 AND PO_2 = 5) scaleXsex = 16.
IF(IDE_8 = 2 AND PO_2 = 6) scaleXsex = 17.
IF(IDE_8 = 2 AND PO_2 = 7) scaleXsex = 18.
IF(IDE_8 = 2 AND PO_2 = 8) scaleXsex = 19.
IF(IDE_8 = 2 AND PO_2 = 9) scaleXsex = 20.
IF(IDE_8 = 2 AND PO_2 = 10) scaleXsex = 21.
IF(IDE_8 = 2 AND PO_2 = 11) scaleXsex = 22.

CROSSTABS scaleXsex by IDE_8.
CROSSTABS scaleXsex by PO_2.


*Add weighs.
compute weight = 1.
IF(scaleXsex=		1	) weight = 	1.54532201217558	.
IF(scaleXsex=		2	) weight = 	0.527061305137242	.
IF(scaleXsex=		3	) weight = 	1.30950283028944	.
IF(scaleXsex=		4	) weight = 	1.01465735790935	.
IF(scaleXsex=		5	) weight = 	1.06910178361636	.
IF(scaleXsex=		6	) weight = 	1.08847086175066	.
IF(scaleXsex=		7	) weight = 	1.16864663692603	.
IF(scaleXsex=		8	) weight = 	0.990915174143993	.
IF(scaleXsex=		9	) weight = 	0.71354730281175	.
IF(scaleXsex=		10	) weight = 	0.726746902702125	.
IF(scaleXsex=		11	) weight = 	0.883869130264516	.
IF(scaleXsex=		12	) weight = 	1.43775505063227	.
IF(scaleXsex=		13	) weight = 	0.564876582906829	.
IF(scaleXsex=		14	) weight = 	0.716625226341229	.
IF(scaleXsex=		15	) weight = 	1.30302534132702	.
IF(scaleXsex=		16	) weight = 	1.79367704429311	.
IF(scaleXsex=		17	) weight = 	0.97100592466318	.
IF(scaleXsex=		18	) weight = 	1.05716156985239	.
IF(scaleXsex=		19	) weight = 	0.840295209221781	.
IF(scaleXsex=		20	) weight = 	0.91757051452754	.
IF(scaleXsex=		21	) weight = 	0.651424924849279	.
IF(scaleXsex=		22	) weight = 	1.19226312505571	.
EXECUTE.

*Assigning weights to political parties.
COMPUTE preference = PV_4.

VALUE LABELS preference
4 "ČSSD"
5 "ODS"
6 "KSČM"
7 "KDU-ČSL"
8 "SZ"
9 "TOP 09"
11 "SPOZ"
13 "ANO"
15 "žádná strana"
16 "jiná strana"
17 "Česká pirátská strana"
18 "Strana svobodných občanů"
19 "Svoboda a přímá demokracie"
21 "STAN"
999 "Rozhodně nepůjde k volbám / Žádný kandidát"
99 "neví".


USE ALL.
COMPUTE filter_vm=(PV_1 = 1 OR PV_1 = 2).
Filter BY filter_vm.
Execute.

if (PV_1 EQ 4) preference = 999.

if(PV_4 EQ 15) preference = 999.
execute.


MISSING VALUES preference (99, 999).
VARIABLE LABELS preference "Volební model - zvážený na subjektivní levopravé zařazení a pohlaví".
WEIGHT by weight.
FREQUENCIES preference.

* Results without weighting.
weight off.
variable labels preference "Volební model - bez vážení".
FREQUENCIES preference.


