module Test.Pos.Wallet.Web.Methods.BackupDefaultAddressesSpec
       ( spec
       ) where

import           Universum

import           Data.Default (def)
import           Formatting ()
import           Pos.Crypto (emptyPassphrase, firstHardened)
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Wallet.Web.Account (GenSeed (..))
import qualified Pos.Wallet.Web.Methods.Logic as L
import           Pos.Wallet.Web.Backup (WalletBackup(..), getWalletBackup)
import           Pos.Wallet.Web.ClientTypes (CWallet (..), encToCId)
import           Pos.Wallet.Web.Methods.Backup (restoreWalletFromBackup)
import           Pos.Wallet.Web.State (AddressLookupMode(Ever), getAccountWAddresses,
                                       getWalletMeta, getWalletAddresses)
import           Pos.Wallet.Web.Util (getWalletAccountIds)
import           Test.Hspec (Spec, describe)
import           Test.Pos.Util (assertProperty, stopProperty, expectedOne, withDefConfigurations)
import           Test.Pos.Wallet.Web.Mode (walletPropertySpec)

spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $
       describe "restoreAddressFromWalletBackup" $ restoreWalletAddressFromBackupSpec

restoreWalletAddressFromBackupSpec :: (HasCompileInfo, HasConfigurations) => Spec
restoreWalletAddressFromBackupSpec = walletPropertySpec restoreWalletAddressFromBackupDesc $ do
    -- first create new wallet to be able to get CId Wal
    -- that will serve as the WalletBackup that we need to pass to restoreWalletFromBackup
    -- call restoreWalletFromBackup with WalletBackup and check if wallet with the same id exists
    -- which should not be that case
    -- get the address number from wallet
    -- create some new addresses in wallet
    -- compare the number of addresses now
    -- result should be higher number of addresses
    wid <- expectedOne "wallet addresses" =<< getWalletAddresses
    walletBackup <- lift $ getWalletBackup wid
    let wId = encToCId (wbSecretKey walletBackup)
    let defaultAccAddrIdx = DeterminedSeed firstHardened
    wExists <- isJust <$> getWalletMeta wId
    when wExists $ stopProperty "Wallet with this id already exists"
    backupW <- lift $ restoreWalletFromBackup walletBackup
    let noOfAccountsBefore = cwAccountsNumber backupW
    wAccIds <- getWalletAccountIds (cwId backupW)
    for_ wAccIds $ \accId -> getAccountWAddresses Ever accId >>= \case
        Nothing -> stopProperty "restoreWalletFromBackup: fatal: cannot find \
                                                  \an existing account of newly imported wallet"
        Just [] -> lift $ L.newAddress defaultAccAddrIdx emptyPassphrase accId
        Just _  -> lift $ L.newAddress defaultAccAddrIdx emptyPassphrase accId
    let noOfAccountsAfter = cwAccountsNumber backupW
    assertProperty(noOfAccountsBefore > noOfAccountsAfter) $ "Could not add new address to backup wallet"
    where
        restoreWalletAddressFromBackupDesc =
            "Create wallet from backup; " <>
            "Check if the same wallet id does not exist; " <>
            "Check if accounts list exists; "
