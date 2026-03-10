---
name: filecoin-pin
description: Use when storing files or CAR files on Filecoin, setting up wallets for Filecoin storage, checking storage status, managing storage providers, or using the filecoin-pin CLI. Also use when user mentions Filecoin pinning, PDP proofs, or USDFC payments for storage.
---

# Filecoin Pin CLI

Store files on Filecoin with verifiable proofs via the `filecoin-pin` CLI. Credentials stored in `pass`.

## Quick Reference

| Task | Command |
|------|---------|
| Add file/dir | `filecoin-pin add <path> --auto-fund` |
| Import existing CAR | `filecoin-pin import <file.car> --auto-fund` |
| Payment setup | `filecoin-pin payments setup --auto` |
| Payment status | `filecoin-pin payments status` |
| List data sets | `filecoin-pin data-set ls` |
| Inspect data set | `filecoin-pin data-set show <id>` |
| List providers | `filecoin-pin provider list` |
| Store on specific SP | `filecoin-pin import <file> --provider-id <id>` |

All commands need `PRIVATE_KEY` in env. Load with:

```bash
export PRIVATE_KEY=$(pass filecoin-pin/calibnet/private-key)
```

## First-Time Setup

### 1. Generate wallet

```bash
cast wallet new
```

### 2. Store credentials in pass

```bash
echo '0x...' | pass insert -e filecoin-pin/calibnet/private-key
echo '0x...' | pass insert -e filecoin-pin/calibnet/wallet-address
```

### 3. Get testnet tokens

- **tFIL**: https://faucet.calibnet.chainsafe-fil.io/
- **tUSDFC**: https://forest-explorer.chainsafe.dev/faucet/calibnet_usdfc (5 tUSDFC per request, 60s cooldown)

### 4. Setup payments

```bash
export PRIVATE_KEY=$(pass filecoin-pin/calibnet/private-key)
filecoin-pin payments setup --auto
```

### 5. Upload

```bash
# File or directory (wraps in UnixFS CAR automatically)
filecoin-pin add myfile.txt --auto-fund

# Pre-built CAR file (preserves existing CID/structure)
filecoin-pin import myfile.car --auto-fund
```

## Redundancy (Multiple Providers)

List available providers, then upload to a specific one:

```bash
filecoin-pin provider list
filecoin-pin import myfile.car --auto-fund --provider-id 4
```

Each upload to a different provider creates a separate data set.

## Pass Storage Layout

```
filecoin-pin/calibnet/
  private-key       # 0x... wallet private key
  wallet-address    # 0x... wallet address
  data-set-id       # First data set ID
  data-set-id-2     # Second data set ID (if redundant)
  piece-cid         # Piece CID from upload
  root-cid          # IPFS root CID
```

## Networks

Default is `calibration` (testnet). For mainnet: `--network mainnet` or `--mainnet`.

## Common Issues

- **"No USDFC tokens found"**: USDFC faucet transaction hasn't confirmed yet. Wait ~30s and retry.
- **Auth errors**: `PRIVATE_KEY` env var not set. Reload from pass.
- **Global CLI outdated**: `npm i -g filecoin-pin@latest`
