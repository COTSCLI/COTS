# 🏁 COTS CLI — Exemples de commandes (pro, simple, structuré)

## Démarrage rapide (workspace par réseau)

1) Initialiser un workspace par réseau (par défaut: Preprod):

```bash
cotscli init --network Preprod
# => crée ~/.cotscli/preprod/{keys,addresses,utxos,transactions,protocol,scripts} + config.json
```

2) Récupérer les paramètres de protocole (Koios) et les stocker:

```bash
cotscli protocol fetch \
  --url https://api.koios.rest/api/v1/epoch_params \
  --out-file ~/.cotscli/preprod/protocol/epoch_params.json \
  --db-file ~/.cotscli/preprod/cots.db
```

3) Générer des clés et une adresse de paiement (+ solde initial dans config):

```bash
# clés
cotscli address key-gen \
  --verification-key-file payment.vkey \
  --signing-key-file payment.skey

# adresse + UTXO genesis (1 ADA) ajouté dans config.json
cotscli address build \
  --payment-verification-key-file payment.vkey \
  --network Preprod \
  --out-file ~/.cotscli/preprod/addresses/alice.addr \
  --initial-amount 1000000 \
  --config ~/.cotscli/preprod/config.json
```

4) Initialiser la base SQLite et importer la config automatiquement:

```bash
cotscli database init --db-file ~/.cotscli/preprod/cots.db
```

---

## UTXO

- Lister les UTXO (non dépensés) depuis la base:

```bash
cotscli utxo list --utxo-file exported-utxos.json  # via export (voir plus bas)
```

- Réserver des UTXO pour un montant donné:

```bash
cotscli utxo reserve \
  --address $(cat ~/.cotscli/preprod/addresses/alice.addr) \
  --amount 2000000 \
  --utxo-file exported-utxos.json \
  --out-file reserved-utxos.json
```

- Exporter/Importer des UTXO (JSON):

```bash
cotscli database export-utxo \
  --db-file ~/.cotscli/preprod/cots.db \
  --out-file exported-utxos.json

cotscli database import-utxo \
  --db-file ~/.cotscli/preprod/cots.db \
  --utxo-file imported-utxos.json
```

---

## Transactions (offline)

- Construire:

```bash
cotscli transaction build \
  --tx-in <TXID>#<TXIX> \
  --tx-out $(cat ~/.cotscli/preprod/addresses/alice.addr)+1000000 \
  --change-address $(cat ~/.cotscli/preprod/addresses/alice.addr) \
  --db-file ~/.cotscli/preprod/cots.db \
  --out-file tx.raw
```

- Signer (offline):

```bash
cotscli transaction sign \
  --tx-file tx.raw \
  --signing-key-file payment.skey \
  --out-file tx.signed
```

- Simuler et valider:

```bash
cotscli transaction simulate --tx-file tx.signed --db-file ~/.cotscli/preprod/cots.db
cotscli transaction validate --tx-file tx.signed --db-file ~/.cotscli/preprod/cots.db
```

- Exporter / Décoder:

```bash
cotscli transaction export --tx-file tx.signed --format CardanoCLI --out-file tx.cli
cotscli transaction decode --tx-file tx.signed --verbose
```

---

## Wallets

```bash
cotscli wallet create --name alice --address $(cat ~/.cotscli/preprod/addresses/alice.addr) --db-file ~/.cotscli/preprod/cots.db
cotscli wallet list --db-file ~/.cotscli/preprod/cots.db
cotscli wallet info --name alice --db-file ~/.cotscli/preprod/cots.db
cotscli wallet export --name alice --db-file ~/.cotscli/preprod/cots.db --file alice.wallet.json
cotscli wallet import --file alice.wallet.json --db-file ~/.cotscli/preprod/cots.db
```

---

## Adresses de staking et Mint (optionnel)

```bash
# Stake
cotscli stake-address key-gen --verification-key-file stake.vkey --signing-key-file stake.skey
cotscli stake-address build --stake-verification-key-file stake.vkey --network Preprod --out-file stake.addr

# Mint
cotscli mint build --out-file mint.raw --network Preprod --protocol-params-file ~/.cotscli/preprod/protocol/epoch_params.json
cotscli mint calculate --policy-id <POLICY> --asset-name TOKEN --quantity 100 --protocol-params-file ~/.cotscli/preprod/protocol/epoch_params.json
```

---

## Paramètres de protocole

```bash
# Récupérer depuis Koios et stocker
cotscli protocol fetch \
  --url https://api.koios.rest/api/v1/epoch_params \
  --out-file ~/.cotscli/preprod/protocol/epoch_params.json \
  --db-file ~/.cotscli/preprod/cots.db

# Mettre à jour depuis un fichier local
cotscli protocol update \
  --protocol-params-file ~/.cotscli/preprod/protocol/epoch_params.json \
  --db-file ~/.cotscli/preprod/cots.db
```

---

## Base de données

```bash
cotscli database inspect --db-file ~/.cotscli/preprod/cots.db
cotscli database snapshot --db-file ~/.cotscli/preprod/cots.db --out-file snapshot.db
cotscli database load-snapshot --snapshot-file snapshot.db --db-file ~/.cotscli/preprod/cots.db
cotscli database reset --db-file ~/.cotscli/preprod/cots.db
```

---

## Aide

- Tous les sous-commandes exposent `--help`.
- Réseaux supportés pour `--network`: `Mainnet`, `Testnet`, `Preview`, `Preprod` (par défaut: `Preprod`).
- Les montants sont en lovelace (1 ADA = 1_000_000 lovelace).
