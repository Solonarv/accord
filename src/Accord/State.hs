module Accord.State where

import           Network.Discord.Types

data AccordState
  = ServerSelect
  | DirectMsgSelect
  | InDMChannel !Snowflake
  | ServerChannelSelect !Snowflake
  | InServerChannel !Snowflake
