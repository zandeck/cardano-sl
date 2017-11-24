module Test.Pos.Wallet.Web.Methods.BackupDefaultAddressesSpec
       ( spec
       ) where

import           Universum

import           Control.Lens (each)
import           Data.Default (def)
import           qualified Data.HashMap.Strict as HM
import           Formatting ()
import           Pos.Client.KeyStorage (addSecretKey)
import           Pos.Crypto (emptyPassphrase, firstHardened)
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Wallet.Web.Account (GenSeed(..))
import           Pos.Wallet.Web.Backup (AccountMetaBackup(..), WalletBackup(..),
                                        WalletMetaBackup(..), getWalletBackup)
import           Pos.Wallet.Web.ClientTypes (CAccountInit(..), CAccountMeta(..),
                                             encToCId, cwAccountsNumber)
import           Pos.Wallet.Web.Methods.Logic (createWalletSafe, newAddress,
                                               getWallet, newAccountIncludeUnready)
import           Pos.Wallet.Web.State (AddressLookupMode(Ever), getAccountWAddresses,
                                       getWalletAddresses)
import           Pos.Wallet.Web.Tracking.Sync (syncWalletOnImport)
import           Pos.Wallet.Web.Util (getWalletAccountIds)
import           Test.Hspec (Spec, describe)
import           Test.Pos.Util (assertProperty, stopProperty, expectedOne,
                                withDefConfigurations)
import           Test.Pos.Wallet.Web.Mode (walletPropertySpec)

spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $
       describe "createAddressFromWalletBackup" $ createWalletAddressFromBackupSpec

createWalletAddressFromBackupSpec :: (HasCompileInfo, HasConfigurations) => Spec
createWalletAddressFromBackupSpec = walletPropertySpec createWalletAddressFromBackupDesc $ do
    -- passphrase <- importSingleWallet mostlyEmptyPassphrases
    wid <- expectedOne "wallet addresses" =<< getWalletAddresses
    walletBackup <- lift $ getWalletBackup wid
    let wId = encToCId (wbSecretKey walletBackup)
    do
        let (WalletMetaBackup wMeta) = wbMeta walletBackup
            accList = HM.toList (wbAccounts walletBackup)
                        & each . _2 %~ \(AccountMetaBackup am) -> am
            defaultAccAddrIdx = DeterminedSeed firstHardened

        lift $ addSecretKey (wbSecretKey walletBackup)
        if null accList
            then do
                let accMeta = CAccountMeta { caName = "Initial account" }
                    accInit = CAccountInit { caInitWId = wId, caInitMeta = accMeta }
                lift $ () <$ newAccountIncludeUnready True defaultAccAddrIdx emptyPassphrase accInit
            else stopProperty "Accounts list should be empty"

        void $ lift $ createWalletSafe wId wMeta False
        void $ lift $ syncWalletOnImport (wbSecretKey walletBackup)
        wAccIds <- getWalletAccountIds wId
        for_ wAccIds $ \accId -> getAccountWAddresses Ever accId >>= \case
            Nothing -> stopProperty "Account Id list is Nothing"
            Just [] -> lift $ newAddress defaultAccAddrIdx emptyPassphrase accId
            Just _  -> stopProperty "Account Id list already has elements"
        wallet <- lift $ getWallet wId
        assertProperty ((cwAccountsNumber wallet) > 0) "are the addresses created for backup wallet ?"
        where
            createWalletAddressFromBackupDesc =
                "Create wallet from backup; " <>
                "Then create wallet accounts; " <>
                "Finally generate addresses for wallet accounts; "
