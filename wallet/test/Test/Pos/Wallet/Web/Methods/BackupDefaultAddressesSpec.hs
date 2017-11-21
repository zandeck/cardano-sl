module Test.Pos.Wallet.Web.Methods.BackupDefaultAddressesSpec
       ( spec
       ) where

import           Universum

import           Data.Default (def)
import           Formatting ()
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Discard (..), arbitrary)
import           Test.QuickCheck.Monadic (pick, stop)

import           Pos.Client.Txp.Addresses (getFakeChangeAddress, getNewAddress)
import           Pos.Core.Address (Address)
import           Pos.Crypto (PassPhrase)
import           Pos.Crypto (emptyPassphrase, firstHardened)
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Wallet.Web.Account (GenSeed(..), genUniqueAccountId, genUniqueAddress)
import           Pos.Wallet.Web.Backup (AccountMetaBackup (..), TotalBackup (..), WalletBackup (..),
                                        WalletMetaBackup (..), getWalletBackup)
import           Pos.Wallet.Web.ClientTypes (AccountId, CAccountInit (..),
                                             CAccountMeta (..), CFilePath (..),
                                             CId, CWallet, Wal, encToCId,
                                             caId, cwamId)

import           Pos.Wallet.Web.Error (WalletError(..))
import           Pos.Wallet.Web.Methods.Logic (newAccount, newAddress)
import           Pos.Wallet.Web.State (getWalletAddresses, getAccountWAddresses)
import           Pos.Wallet.Web.State (AddressLookupMode(Ever), createAccount,
                                       getAccountWAddresses, getWalletMeta,
                                       getWalletAddresses)
import           Pos.Wallet.Web.Util (decodeCTypeOrFail)
import           Pos.Wallet.Web.Util (getWalletAccountIds)
import           Test.Pos.Util (assertProperty, expectedOne,
                                withDefConfigurations)
import           Test.Pos.Wallet.Web.Mode (walletPropertySpec)
import           Test.Pos.Wallet.Web.Mode (WalletProperty)
import           Test.Pos.Wallet.Web.Util (importSingleWallet,
                                           mostlyEmptyPassphrases)

-- TODO remove HasCompileInfo when MonadWalletWebMode will be splitted.

type AddressGenerator = AccountId -> PassPhrase -> WalletProperty Address

spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $
       describe "Wallet.Web.Methods.BackupDefaultAddresses" $
       modifyMaxSuccess (const 10) $ do
        prop "sasa" $
           createWalletAddressFromBackupSpec commonAddressGenerator

createWalletAddressFromBackupSpec :: (HasCompileInfo, HasConfigurations) => AddressGenerator -> Word32 -> WalletProperty ()
createWalletAddressFromBackupSpec generator accSeed = do
    let defaultAccAddrIdx = DeterminedSeed firstHardened
    passphrase <- importSingleWallet mostlyEmptyPassphrases
    wid <- expectedOne "wallet addresses" =<< getWalletAddresses
    accId <- lift $ decodeCTypeOrFail . caId
         =<< newAccount (DeterminedSeed accSeed) passphrase (CAccountInit def wid)
    ai <- newAccount (DeterminedSeed accSeed) passphrase (CAccountInit def wid)
    let walletBackup = getWalletBackup ai
    let wId = encToCId walletBackup
    -- wAccIds <- getWalletAccountIds wId
    -- wAccIds $ getAccountWAddresses Ever accId >>= \case
	-- 			Nothing -> throwM $ InternalError "restoreWalletFromBackup: fatal: cannot find \
	-- 											  \an existing account of newly imported wallet"
	-- 			Just [] -> void $ newAddress defaultAccAddrIdx emptyPassphrase accId
	-- 			Just _  -> pure ()

    getWallet wId
    assertProperty (True == True) "is address created for backup wallet ?"

commonAddressGenerator :: HasConfigurations => AddressGenerator
commonAddressGenerator accId passphrase = do
    addrSeed <- pick arbitrary
    let genAddress = genUniqueAddress (DeterminedSeed addrSeed) passphrase accId
    -- can't catch under 'PropertyM', workarounding
    maddr <- lift $ (Just <$> genAddress) `catch` seedBusyHandler
    addr <- maybe (stop Discard) pure maddr
    lift $ decodeCTypeOrFail (cwamId addr)
  where
    seedBusyHandler (InternalError "address generation: this index is already taken")
                      = pure Nothing
    seedBusyHandler e = throwM e
