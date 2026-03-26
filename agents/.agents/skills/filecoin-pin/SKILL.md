---
name: filecoin-pin
description: Use when storing files or CAR files on Filecoin, setting up wallets for Filecoin storage, checking storage status, managing storage providers, or using the filecoin-pin CLI. Also use when user mentions Filecoin pinning, PDP proofs, or USDFC payments for storage.
license: Apache-2.0 OR MIT
metadata:
  author: filecoin-project
  version: "0.18"
---

# Filecoin Pin CLI

Store files on Filecoin with verifiable proofs via the `filecoin-pin` CLI.

## Quick Reference

| Task | Command |
|------|---------|
| Add file/dir | `filecoin-pin add <path> --auto-fund` |
| Import existing CAR | `filecoin-pin import <file.car> --auto-fund` |
| Payment setup | `filecoin-pin payments setup --auto` |
| Payment status | `filecoin-pin payments status` |
| List data sets | `filecoin-pin data-set ls` |
| Inspect data set | `filecoin-pin data-set show <id>` |
| Terminate data set | `filecoin-pin data-set terminate <id>` |
| List providers | `filecoin-pin provider list` |
| Store on specific SPs | `filecoin-pin import <file> --provider-ids 1,4` |

All commands need `PRIVATE_KEY` in env. Load it from your secret store:

```bash
# Examples — use whichever secret manager your environment provides:
export PRIVATE_KEY=$(pass filecoin-pin/calibnet/private-key)          # pass (Unix)
export PRIVATE_KEY=$(op read "op://Vault/filecoin-pin/private-key")   # 1Password CLI
export PRIVATE_KEY=$(security find-generic-password -s filecoin-pin -w) # macOS Keychain
export PRIVATE_KEY=$(vault kv get -field=key secret/filecoin-pin)     # HashiCorp Vault
```

## First-Time Setup

### 1. Generate wallet

```bash
cast wallet new
```

### 2. Store credentials

Save the private key and wallet address in your preferred secret manager. For example, with `pass`:

```bash
echo '0x...' | pass insert -e filecoin-pin/calibnet/private-key
echo '0x...' | pass insert -e filecoin-pin/calibnet/wallet-address
```

### 3. Get testnet tokens

- **tFIL**: https://faucet.calibnet.chainsafe-fil.io/
- **tUSDFC**: https://forest-explorer.chainsafe.dev/faucet/calibnet_usdfc (5 tUSDFC per request, 60s cooldown)

### 4. Setup payments

```bash
export PRIVATE_KEY=<load from your secret store>
filecoin-pin payments setup --auto
```

### 5. Upload

```bash
# File or directory (wraps in UnixFS CAR automatically)
filecoin-pin add myfile.txt --auto-fund

# Pre-built CAR file (preserves existing CID/structure)
filecoin-pin import myfile.car --auto-fund
```

## Redundancy (Multiple Copies)

By default, `add` and `import` create **2 copies** across different storage providers for durability. Each copy is independently verified on-chain. Data is only uploaded once regardless of copy count.

```bash
# Default: 2 copies across different providers
filecoin-pin add myfile.txt --auto-fund

# Explicit copy count
filecoin-pin add myfile.txt --auto-fund --count 3

# Single copy (no redundancy)
filecoin-pin import myfile.car --auto-fund --count 1

# Target specific providers (comma-separated)
filecoin-pin import myfile.car --auto-fund --provider-ids 1,4

# Target existing data sets (comma-separated)
filecoin-pin import myfile.car --auto-fund --data-set-ids 10,20

# List available providers
filecoin-pin provider list
```

Note: `--provider-ids` and `--data-set-ids` are mutually exclusive. The old `--provider-id` (singular) still works for single-provider override.

## Credential Storage

Keep private keys out of shell history and dotfiles. Use a secret manager and load into env only when needed. Suggested layout (adapt paths to your tool):

```
filecoin-pin/calibnet/
  private-key       # 0x... wallet private key
  wallet-address    # 0x... wallet address
  data-set-id       # First data set ID
  piece-cid         # Piece CID from upload
  root-cid          # IPFS root CID
```

## Networks

Default is `calibration` (testnet). For mainnet: `--network mainnet` or `--mainnet`. For local devnet: `--network devnet`.

## Common Issues

- **"No USDFC tokens found"**: USDFC faucet transaction hasn't confirmed yet. Wait ~30s and retry.
- **Auth errors**: `PRIVATE_KEY` env var not set. Reload from your secret store.
- **Global CLI outdated**: `npm i -g filecoin-pin@latest`
