Import matched bets to MSSQL database (http://data.betfair.com/)
================================

1) Create SQL schema for tennis matched prices
----------------------------
CREATE TABLE [dbo].[betfair_data](
	[SPORTS_ID] [varchar](50) NOT NULL,
	[EVENT_ID] [int] NOT NULL,
	[SETTLED_DATE] [datetime] NULL,
	[FULL_DESCRIPTION] [varchar](255) NOT NULL,
	[SCHEDULED_OFF] [datetime] NOT NULL,
	[EVENT] [varchar](50) NOT NULL,
	[DT_ACTUAL_OFF] [datetime] NULL,
	[SELECTION_ID] [int] NOT NULL,
	[SELECTION] [varchar](50) NOT NULL,
	[ODDS] [decimal](18, 2) NOT NULL,
	[NUMBER_BETS] [int] NOT NULL,
	[VOLUME_MATCHED] [decimal](18, 2) NOT NULL,
	[LATEST_TAKEN] [datetime] NOT NULL,
	[FIRST_TAKEN] [datetime] NOT NULL,
	[WIN_FLAG] [bit] NULL,
	[IN_PLAY] [varchar](50) NOT NULL
) ON [PRIMARY]

2) Import tennis matched prices for years 2010-2011 from http://data.betfair.com to MSQL database
------------------------------------------------------------
SET DATEFORMAT dmy

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_091228to100103_100106121826.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100104to100110_100113121340.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100111to100117_100120122529.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100118to100124_100127122034.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100125to100131_100203122220.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100201to100207_100210122335.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100208to100214_100217122308.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100215to100221_100224122333.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100222to100228_100303121930.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100301to100307_100310122824.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100308to100314_100317122319.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100315to100321_100324123349.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100322to100328_100331123223.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100329to100404_100407122535.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100405to100411_100414124207.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100412to100418_100421122649.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100419to100425_100428123201.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100426to100502_100505122818.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100503to100509_100512122950.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100510to100516_100519123012.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100517to100523_100526122253.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100524to100530_100602124222.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100531to100606_100609123734.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100607to100613_100616123120.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100614to100620_100623124212.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100621to100627_100630124321.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100628to100704_100707134621.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100705to100711_100714122544.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100712to100718_100721122155.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100719to100725_100728123344.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100726to100801_100804123333.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100802to100808_100811122606.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100809to100815_100818123038.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100816to100822_100825122847.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100823to100829_100901122559.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100830to100905_100908122601.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100906to100912_100915122544.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100913to100919_100922122411.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100920to100926_100929122510.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_100927to101003_101006122632.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_101004to101010_101013122658.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_101011to101017_101020123037.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_101018to101024_101027122145.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_101025to101031_101103123340.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_101101to101107_101110123049.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_101108to101114_101117122733.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_101115to101121_101124122510.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_101122to101128_101201123155.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_101129to101205_101208122426.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_101206to101212_101215122455.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_101213to101219_101222122436.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_101220to101226_101229121037.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_101227to110102_110105122658.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110103to110109_110112122403.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110110to110116_110119123330.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110117to110123_110126124204.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110124to110130_110202123844.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110131to110206_110209122803.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110207to110213_110216124058.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110214to110220_110223124630.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110221to110227_110302123939.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110228to110306_110309124527.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110307to110313_110316123414.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110314to110320_110323123406.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110321to110327_110330122837.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110328to110403_110407072957.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110404to110410_110413123802.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110411to110417_110420130911.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110418to110424_110427123618.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110425to110501_110504125752.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110502to110508_110511131307.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110509to110515_110518122852.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110516to110522_110525123017.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110523to110529_110601122223.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110530to110605_110608123956.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110606to110612_110615124902.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110613to110619_110622130711.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110620to110626_110629125140.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110627to110703_110706200900.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110704to110710_110713131325.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110711to110717_110720124419.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110718to110724_110727125739.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110725to110731_110803125936.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110801to110807_110810121820.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110808to110814_110817122410.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110815to110821_110824130733.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110822to110828_110831122443.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110829to110904_110907122604.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110905to110911_110914122022.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110912to110918_110921122809.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110919to110925_110928122742.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_110926to111002_111005122556.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_111003to111009_111012123428.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_111010to111016_111019122347.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_111017to111023_111026122200.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_111024to111030_111102124636.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_111031to111106_111109131402.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_111107to111113_111116131033.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_111114to111120_111123132429.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_111121to111127_111130131242.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_111128to111204_111207131305.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_111205to111211_111214124015.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_111212to111218_111221132057.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_111219to111225_111228123753.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)
BULK INSERT betfair_data FROM 'c:\daniel\betfair_data\bfinf_other_111226to120101_120104125830.csv' WITH(DATAFILETYPE = 'char',FIELDTERMINATOR = '","',ROWTERMINATOR = '0x0a',FIRSTROW=2)

--- Remove quote <"> characters from sport_ids and in_play fields
update betfair_data Set SPORTS_ID = REPLACE(SPORTS_ID, CHAR(34), ''), IN_PLAY = REPLACE(IN_PLAY, CHAR(34), '')

3) Keep pre_play bets on 'Match Odds' markets only
delete from betfair_data where sports_id!=2 or event!='Match Odds' or SETTLED_DATE IS NULL or DT_ACTUAL_OFF IS NULL or WIN_FLAG IS NULL or LATEST_TAKEN >= DT_ACTUAL_OFF