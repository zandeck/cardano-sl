module Test.Pos.Wallet.Web.Methods.BackupDefaultAddressesSpec
       ( spec
       ) where

import           Universum

import           Data.Default (def)
import           Formatting ()
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)

import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Launcher (HasConfigurations)
import           Test.Pos.Util (withDefConfigurations)
import           Test.Pos.Wallet.Web.Mode (walletPropertySpec)
import           Test.Pos.Wallet.Web.Util ()




-- TODO remove HasCompileInfo when MonadWalletWebMode will be splitted.
spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $
       describe "Wallet.Web.Methods.BackupDefaultAddresses" $ modifyMaxSuccess (const 10) $ do
    describe "create default address from wallet backup" $ do
        describe "create address from backup" createWalletAddressFromBackupSpec

createWalletAddressFromBackupSpec :: (HasCompileInfo, HasConfigurations) => Spec
createWalletAddressFromBackupSpec = walletPropertySpec createWalletAddressFromBackupDesc $ do
    error "not implemented"
    where
        createWalletAddressFromBackupDesc = "Create wallet from backup; " <>
                                            "Generate default wallet addresses; "
