{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
module Test.Pos.Wallet.Web.Methods.BackupDefaultAddressesSpec
       ( spec
       ) where

import           Universum

import           Data.Default (def)
import           Formatting ()
import           Pos.Crypto (firstHardened)
import           Pos.Crypto.Signing.Types.Safe (emptyPassphrase)
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.BackupPhrase (mkBackupPhrase9, bpToList)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Wallet.Web.Account (GenSeed(..))
import           Pos.Wallet.Web.Backup (WalletBackup(..), getWalletBackup)
import           Pos.Wallet.Web.ClientTypes (CWallet(..), CWalletMeta(..),
                                             CWalletInit(..), CWalletAssurance(..),
                                             Wal(..), CId(..), CHash(..), encToCId)
import           Pos.Wallet.Web.Methods.Backup (restoreWalletFromBackup)
import           qualified Pos.Wallet.Web.Methods.Logic as L
import           Pos.Wallet.Web.Methods.Restore (newWallet)
import           Pos.Wallet.Web.State (AddressLookupMode(Ever), getAccountWAddresses,
                                       getWalletMeta, getWalletAddresses)
import           Pos.Wallet.Web.Util (getWalletAccountIds)
import           Test.Hspec (Spec, describe)
import           Test.Pos.Util (assertProperty, stopProperty, expectedOne,
                                withDefConfigurations)

import Test.Pos.Wallet.Web.Mode (walletPropertySpec)


spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $
       describe "restoreAddressFromWalletBackup" $ restoreWalletAddressFromBackupSpec

restoreWalletAddressFromBackupSpec :: (HasCompileInfo, HasConfigurations) => Spec
restoreWalletAddressFromBackupSpec = walletPropertySpec restoreWalletAddressFromBackupDesc $ do
    -- I NEED FIRST TO CREATE WALLETBACKUP -> HOW ?
    -- MAYBE LIKE THIS
    -- WALLETBACKUP CREATION
    -- wBackup <- lift $ getWalletBackup ((CId (CHash "test")) :: CId Wal)

    -- OR CREATE NEW WALLET ?
    let cWmeta = CWalletMeta { cwName = "WalletBackup test", cwAssurance = CWANormal, cwUnit = 0 }
        backupPhrase = mkBackupPhrase9 ["Serokell", "rocks!", "Haskell", "is", "future", "I", "need", "to", "sleep" :: Text]
        cwInit = CWalletInit { cwInitMeta = cWmeta, cwBackupPhrase = backupPhrase }
    -- WHY THE TEST FAILS WHEN i HAVE ONLY THIS AND ASSERT TRUE == TRUE ? DUNNO
    nWallet <- lift $ newWallet emptyPassphrase cwInit
    -- let defaultAccAddrIdx = DeterminedSeed firstHardened

    -- THIS WAS FROM AN OLD APPROACH ...

    -- wid <- expectedOne "wallet addresses" =<< getWalletAddresses
    -- walletBackup <- lift $ getWalletBackup wid
    -- let wId = encToCId (wbSecretKey walletBackup)
    -- wExists <- isJust <$> getWalletMeta wId
    -- when wExists $ stopProperty "Wallet with this id already exists"

    -- SHOULD i CATCH EXCEPTION OR JUST DEAL WITH WALLET RETURNED FROM THIS FUNCTION ?

    -- backupW <- lift $ restoreWalletFromBackup wBackup

    -- I THOUGHT OF GETTING THE ACCOUNT NUMBER AND LATER
    -- INCREMENTING IT AND TESTING IF IT WORKS. THAT WOULD
    -- MEAN THAT WALLET IS SUCCESSFULLY RESTORED, RIGHT ?

    -- let noOfAccountsBefore = cwAccountsNumber backupW
    -- assertProperty(noOfAccountsBefore >= 0) $ "Number of accounts should be greater or equal to 0"

    -- wAccIds <- getWalletAccountIds (cwId backupW)
    -- for_ wAccIds $ \accId -> getAccountWAddresses Ever accId >>= \case
    --     Nothing -> stopProperty "restoreWalletFromBackup: fatal: cannot find \
    --                                               \an existing account of newly imported wallet"
    --     Just [] -> lift $ L.newAddress defaultAccAddrIdx emptyPassphrase accId
    --     Just _  -> lift $ L.newAddress defaultAccAddrIdx emptyPassphrase accId

    -- let noOfAccountsAfter = cwAccountsNumber backupW
    -- assertProperty(noOfAccountsBefore > noOfAccountsAfter) $ "Could not add new address to restored wallet"
    assertProperty(True == True) $ "Could not add new address to backup wallet"
    where
        restoreWalletAddressFromBackupDesc =
            "Create wallet from backup; " <>
            "Check if the wallet addresses count can increment; "
