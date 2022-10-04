# plutus-project-template


## Prerequisites
**Warning**

Make sure you have nix installed
https://nixos.org/download.html and nix.conf correctly setup
https://github.com/input-output-hk/plutus-apps#nix-1

1. Clone Plutus Apps
```
git clone https://github.com/input-output-hk/plutus-apps
```
2. Git checkout to `tag` you require eg:
```
git checkout v1.0.0-alpha1
```
3. inside plutus-apps folder run nix-shell

```
cardano@4ba98a8eee51:~/plutus-apps$ nix-shell
[nix-shell:~/plutus-apps]$ 
```

## Run project tempalte

1. go to plutus-project-template/`git-chekout-version` like below
```
cd plutus-project-template/v1.0.0-alpha1
```
2. run cabal update
```
cabal update
```
3. run cabal repl
```
cabal repl
```
4. inside cabal repl import OffChainGift
```
λ> import OffChainGift
```
5. execute runEmulator
```
λ> runEmulator 
Slot 00000: TxnValidate 161afa70667cfca2e083d7b65ba860cd3be1c9b2717bd9c80664d4907988e24f
Slot 00000: SlotAdd Slot 1
Slot 00001: W[7]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 3148ce3fae5fefb24df5551dadae50943e20400a47286dda6e9c5a415eaf2d99, BlockNumber 0). UTxO state was added to the end.
.....
.....
.....
Final balances
Wallet 7: 
    {, ""}: 100000000
Wallet 8: 
    {, ""}: 100000000
Wallet 6: 
    {, ""}: 100000000
Wallet 4: 
    {, ""}: 100000000
Wallet 2: 
    {, ""}: 109830668
Wallet 1: 
    {, ""}: 89818879
Wallet 10: 
    {, ""}: 100000000
Wallet 9: 
    {, ""}: 100000000
Wallet 3: 
    {, ""}: 100000000
Wallet 5: 
    {, ""}: 100000000

```