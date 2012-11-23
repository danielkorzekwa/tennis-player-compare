
SQL schema for market probabilities, for example calculated with Glicko2/MarkovChain prediction model
-----------------------------------------------------------------------------------------------------
CREATE TABLE [dbo].[market_prob](
	[market_id] [int] NOT NULL,
	[full_description] [varchar](255) NULL,
	[scheduled_off] [datetime] NULL,
	[selection_id] [int] NOT NULL,
	[selection] [varchar](50) NULL,
	[probability] [decimal](18, 4) NOT NULL,
	[surface] [varchar](50) NULL,
	[match_type] [varchar](50) NULL
) ON [PRIMARY]

