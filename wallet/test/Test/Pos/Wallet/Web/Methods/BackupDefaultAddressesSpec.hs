module Test.Pos.Wallet.Web.Methods.BackupDefaultAddressesSpec
       ( spec
       ) where

import           Universum

import           Data.Default (def)
import           Formatting ()
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Wallet.Web.Backup (WalletBackup(..), getWalletBackup)
import           Pos.Wallet.Web.ClientTypes (encToCId)
import           Pos.Wallet.Web.Methods.Backup (restoreWalletFromBackup)
import           Pos.Wallet.Web.State (AddressLookupMode(Ever), getAccountWAddresses,
                                       getWalletMeta, getWalletAddresses)
import           Pos.Wallet.Web.Util (getWalletAccountIds)
import           Test.Hspec (Spec, describe)
import           Test.Pos.Util (stopProperty, expectedOne, withDefConfigurations)
import           Test.Pos.Wallet.Web.Mode (walletPropertySpec)

spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $
       describe "restoreAddressFromWalletBackup" $ restoreWalletAddressFromBackupSpec

restoreWalletAddressFromBackupSpec :: (HasCompileInfo, HasConfigurations) => Spec
restoreWalletAddressFromBackupSpec = walletPropertySpec restoreWalletAddressFromBackupDesc $ do
    wid <- expectedOne "wallet addresses" =<< getWalletAddresses
    walletBackup <- lift $ getWalletBackup wid
    let wId = encToCId (wbSecretKey walletBackup)
    wExists <- isJust <$> getWalletMeta wId
    when wExists $ stopProperty "Wallet with this id already exists"
    backupW <- lift $ restoreWalletFromBackup walletBackup
    let bwId = encToCId (wbSecretKey backupW)
    wAccIds <- getWalletAccountIds bwId
    for_ wAccIds $ \accId -> getAccountWAddresses Ever accId >>= \case
        Nothing -> stopProperty "restoreWalletFromBackup: fatal: cannot find \
                                                  \an existing account of newly imported wallet"
        Just _  -> pure ()
    where
        restoreWalletAddressFromBackupDesc =
            "Create wallet from backup; " <>
            "Check if the same wallet id does not exist; " <>
            "Check if accounts list exists; "
