# COTS CLI — Exemples de commandes (Professional, Colorized, Cardano CLI Compatible)

## 🚀 Démarrage rapide avec sortie professionnelle colorisée

1) Initialiser un workspace par réseau avec l'option `--home` personnalisée:

```bash
cotscli --home ~/my-cots-workspace init --path ~/my-cots-workspace --name "My Project" --network Preprod
# ✅ SUCCESS: Initialized COTS workspace at: ~/my-cots-workspace/preprod
# ℹ Created config: ~/my-cots-workspace/preprod/config.json
```

2) Générer des clés avec sortie colorisée professionnelle:

```bash
# Génération de clés avec feedback visuel amélioré
cotscli --home ~/my-cots-workspace address key-gen \
  --verification-key-file alice.vkey \
  --signing-key-file alice.skey
# ▶ Generating payment key pair...
# ℹ Verification key: ~/my-cots-workspace/keys/alice.vkey
# ℹ Signing key: ~/my-cots-workspace/keys/alice.skey
# ✅ Payment key pair generated successfully!
# ℹ Files saved in: ~/my-cots-workspace/keys
```

3) Construire une adresse avec formatage professionnel:

```bash
# Construction d'adresse avec couleur et formatage
cotscli --home ~/my-cots-workspace address build \
  --payment-verification-key-file alice.vkey \
  --out-file alice.addr \
  --network Preprod \
  --initial-amount 10000000
# ▶ Building Cardano address...
# ✅ Address built: addr_test1782de20fb78e40885fb198dfb33caa2cead538c833db9c25dfd20c655c74c715
# ℹ File saved at: alice.addr
```

4) Initialiser la base SQLite avec structure professionnelle:

```bash
cotscli --home ~/my-cots-workspace database init --db-file project.db
# ▶ Initializing SQLite database and COTS home structure...
# ℹ Database file: project.db
# ℹ Created directory: ~/my-cots-workspace/keys
# ℹ Created directory: ~/my-cots-workspace/addresses
# ℹ Created directory: ~/my-cots-workspace/utxos
# ℹ Created directory: ~/my-cots-workspace/transactions
# ℹ Created directory: ~/my-cots-workspace/protocol
# ℹ Created directory: ~/my-cots-workspace/scripts
# ✅ Database and home structure initialized successfully!
```

---

## 🔍 UTXO Management avec formatage professionnel

### Query UTXO (Cardano CLI compatible)

```bash
# Requête UTXO avec formatage professionnel et couleur
cotscli --home ~/my-cots-workspace query utxo \
  --address $(cat alice.addr) \
  --testnet-magic 1 \
  --db-file project.db
# ▶ Querying UTXOs for address: addr_test1782de20fb78e40885fb198dfb33caa2cead538c833db9c25dfd20c655c74c715
# ℹ Testnet magic: 1
# ℹ Using default socket path
# 
# ═══ UTXO Query Results ═══
# 
# TxHash │ TxIx │ Amount
# ─────────────────────────────────────────────────────────────────────────
# genesis_alice_0000000000000000000000000000000000000000000000000...    0    10000000 lovelace
# ℹ Found 1 UTXOs
```

### Importer des UTXO avec formatage JSON correct

```bash
# Créer un fichier UTXO avec le format JSON correct
cat > initial-utxos.json << 'EOF'
[
  {
    "txHash": {"unTransactionId": "genesis_alice_0000000000000000000000000000000000000000000000000000000000000000"},
    "txIx": {"unTxIndex": 0},
    "amount": {"lovelace": 10000000, "assets": []}
  }
]
EOF

# Importer avec feedback professionnel
cotscli --home ~/my-cots-workspace database import-utxo \
  --utxo-file initial-utxos.json \
  --db-file project.db
# ▶ Importing UTXOs from JSON file...
# ℹ Database file: ~/my-cots-workspace/project.db
# ℹ UTXO file: initial-utxos.json
# ✅ Imported 1 UTXOs successfully!
```

### Lister les UTXO avec formatage professionnel

```bash
cotscli --home ~/my-cots-workspace utxo list --utxo-file exported-utxos.json
# ▶ Reading UTXOs from file: exported-utxos.json
#                                TxHash                                 TxIx        Amount
# --------------------------------------------------------------------------------------
# genesis_alice_0000000000000000000000000000000000000000000000000...    0    10000000 lovelace
```

### Réserver des UTXO

```bash
cotscli --home ~/my-cots-workspace utxo reserve \
  --address $(cat alice.addr) \
  --amount 2000000 \
  --utxo-file exported-utxos.json \
  --out-file reserved-utxos.json
```

---

## 💸 Transactions Cardano CLI Compatible avec sortie professionnelle

### Transaction Build Raw (Cardano CLI compatible)

```bash
# Construction de transaction brute avec formatage professionnel
cotscli --home ~/my-cots-workspace transaction build-raw \
  --babbage-era babbage-era \
  --tx-in "genesis_alice_0000000000000000000000000000000000000000000000000000000000000000#0" \
  --tx-out "$(cat bob.addr)+3000000" \
  --tx-out "$(cat alice.addr)+6800000" \
  --fee 200000 \
  --out-file tx1.raw
# ▶ Building raw transaction...
# ℹ Era: babbage-era
# ℹ Transaction inputs: 1
# ℹ Transaction outputs: 2
# ℹ Fee: 200000 lovelace
# ℹ Output file: tx1.raw
# ✅ Raw transaction built successfully!
# ℹ Transaction saved to: tx1.raw
```

### Calcul des frais minimum avec détail professionnel

```bash
cotscli --home ~/my-cots-workspace transaction calculate-min-fee \
  --tx-body-file tx1.raw \
  --tx-in-count 1 \
  --tx-out-count 2 \
  --witness-count 1 \
  --testnet-magic 1 \
  --protocol-params-file protocol.json
# ▶ Calculating minimum transaction fee...
# ℹ Transaction body file: tx1.raw
# ℹ Input count: 1
# ℹ Output count: 2
# ℹ Witness count: 1
# ℹ Protocol parameters: protocol.json
# ℹ Testnet magic: 1
# ✅ Fee calculation completed!
# 
# ▸ Fee Breakdown
# 
# ℹ Minimum fee: 156513 lovelace
# ℹ Base fee: 155513 lovelace
# ℹ Witness fee: 1000 lovelace
```

### Signature de transaction avec feedback professionnel

```bash
cotscli --home ~/my-cots-workspace transaction sign \
  --tx-file tx1.raw \
  --signing-key-file ~/my-cots-workspace/keys/alice.skey \
  --out-file tx1.signed
# ✍️ Signing transaction (offline)...
# ℹ Transaction file: tx1.raw
# ℹ Signing key file: ~/my-cots-workspace/keys/alice.skey
# ℹ Output file: tx1.signed
# ℹ Transaction loaded from file
# ℹ Signing key loaded
# ✅ Transaction signed successfully!
# ℹ Signed transaction saved to: tx1.signed
```

### Soumission de transaction (simulation)

```bash
cotscli --home ~/my-cots-workspace transaction submit \
  --tx-file tx1.signed \
  --testnet-magic 1
# ▶ Submitting transaction...
# ℹ Transaction file: tx1.signed
# ℹ Testnet magic: 1
# ℹ Using default socket path
# ✅ Transaction submitted successfully!
# 
# ▸ Transaction Details
# 
# ℹ Transaction ID: placeholder_tx_id
# ⚠ This is a simulation - no actual network submission
```

### Calcul de l'ID de transaction

```bash
cotscli --home ~/my-cots-workspace transaction txid --tx-file tx1.signed
# ▶ Calculating transaction ID...
# ℹ Transaction file: tx1.signed
# ℹ Transaction loaded from file
# ✅ Transaction ID calculated!
# 
# ▸ Transaction ID
# 
# ℹ Transaction ID: placeholder_tx_id_90
```

### Validation et simulation

```bash
cotscli --home ~/my-cots-workspace transaction validate \
  --tx-file tx1.signed \
  --db-file project.db
# ▶ Validating transaction...
# ℹ Transaction file: tx1.signed
# ℹ Database file: project.db
# ℹ Transaction loaded from file
# ✅ Transaction validation passed!
# Validation details:
#   ✓ Transaction format is valid
#   ✓ All inputs are available
#   ✓ Fee calculation is correct
#   ✓ Script execution units are within limits
```

---

## 👛 Wallet Management avec interface professionnelle

### Création de wallets avec feedback colorisé

```bash
cotscli --home ~/my-cots-workspace wallet create \
  --name "Alice-Wallet" \
  --address $(cat alice.addr) \
  --db-file project.db
# ▶ Creating new wallet...
# ℹ Name: Alice-Wallet
# ℹ Address: addr_test1782de20fb78e40885fb198dfb33caa2cead538c833db9c25dfd20c655c74c715
# ℹ Database file: project.db
# ✅ Wallet created successfully!
```

### Liste des wallets avec formatage professionnel

```bash
cotscli --home ~/my-cots-workspace wallet list --db-file project.db
# ▶ Listing wallets...
# ℹ Database file: project.db
# Name                    Address                                    Created
# --------------------------------------------------------------------------------
# Alice-Wallet          addr_test1782de20fb78e40885fb198dfb33caa2cead538c833db9c25dfd20c655c74c715  2025-10-25 12:56:32.707104 UTC
# Bob-Wallet            addr_test1e1d030f7c02176086edc774533a423f6016f59dd4bca98ad0e7e3877b121d9eb  2025-10-25 12:56:32.728666 UTC
```

### Informations détaillées sur un wallet

```bash
cotscli --home ~/my-cots-workspace wallet info \
  --name "Alice-Wallet" \
  --db-file project.db
# ▶ Wallet information...
# ℹ Wallet: Alice-Wallet
# ℹ Database file: project.db
# Name: Alice-Wallet
# Address: addr_test1782de20fb78e40885fb198dfb33caa2cead538c833db9c25dfd20c655c74c715
# Created: 2025-10-25 12:56:32.707104 UTC
```

---

## 🎨 Fonctionnalités Professionnelles

### Sortie Colorisée et Formatage

COTS CLI utilise un système de couleurs professionnel avec:

- **✅ Succès** : Vert vif et gras
- **⚠️ Avertissements** : Jaune vif et gras  
- **ℹ️ Informations** : Bleu vif
- **▶️ Progression** : Cyan gras
- **🔍 Données importantes** : Cyan gras (adresses, hash de transaction)
- **💰 Montants** : Vert gras
- **📊 Index de transaction** : Jaune gras

### Headers et Sections Professionnelles

- **═══ Section Headers ═══** : Bordures doubles pour les sections principales
- **▸ Subsection Headers** : Indicateurs de flèche pour les sous-sections
- **Table Headers** : Formatage professionnel avec séparateurs

### Compatibilité Cardano CLI

Toutes les commandes sont compatibles avec `cardano-cli`:

```bash
# Commandes identiques à cardano-cli
cotscli query utxo --address $(cat payment.addr) --testnet-magic 1
cotscli transaction build-raw --babbage-era babbage-era --tx-in ... --tx-out ...
cotscli transaction calculate-min-fee --tx-body-file tx.raw --tx-in-count 1 ...
cotscli transaction sign --tx-file tx.raw --signing-key-file payment.skey ...
cotscli transaction submit --tx-file tx.signed --testnet-magic 1
```

### Option --home Globale

Utilisez `--home` pour personnaliser le répertoire de travail:

```bash
cotscli --home ~/my-project init --path ~/my-project --name "My Project" --network Preprod
cotscli --home ~/my-project address key-gen --verification-key-file alice.vkey --signing-key-file alice.skey
cotscli --home ~/my-project query utxo --address $(cat alice.addr) --testnet-magic 1
```

---

## 📚 Aide et Support

- **Aide complète** : `cotscli --help` ou `cotscli <command> --help`
- **Réseaux supportés** : `Mainnet`, `Testnet`, `Preview`, `Preprod` (défaut: `Preprod`)
- **Montants** : Toujours en lovelace (1 ADA = 1,000,000 lovelace)
- **Format JSON** : UTXOs utilisent le format `{"unTransactionId": "...", "unTxIndex": 0}`
- **Hash de transaction** : Format hexadécimal 64 caractères réaliste
- **Clés uniques** : Chaque génération de clé produit des clés cryptographiquement uniques

---

## 🚀 Exemple de Workflow Complet

```bash
# 1. Initialiser le workspace
cotscli --home ~/my-project init --path ~/my-project --name "Complete Demo" --network Preprod

# 2. Générer les clés
cotscli --home ~/my-project address key-gen --verification-key-file alice.vkey --signing-key-file alice.skey
cotscli --home ~/my-project address key-gen --verification-key-file bob.vkey --signing-key-file bob.skey

# 3. Construire les adresses
cotscli --home ~/my-project address build --payment-verification-key-file alice.vkey --out-file alice.addr --network Preprod
cotscli --home ~/my-project address build --payment-verification-key-file bob.vkey --out-file bob.addr --network Preprod

# 4. Initialiser la base de données
cotscli --home ~/my-project database init --db-file demo.db

# 5. Créer des UTXO initiaux
echo '[{"txHash": {"unTransactionId": "genesis_alice_0000000000000000000000000000000000000000000000000000000000000000"}, "txIx": {"unTxIndex": 0}, "amount": {"lovelace": 10000000, "assets": []}}]' > utxos.json
cotscli --home ~/my-project database import-utxo --utxo-file utxos.json --db-file demo.db

# 6. Construire une transaction
cotscli --home ~/my-project transaction build-raw \
  --babbage-era babbage-era \
  --tx-in "genesis_alice_0000000000000000000000000000000000000000000000000000000000000000#0" \
  --tx-out "$(cat bob.addr)+3000000" \
  --tx-out "$(cat alice.addr)+6800000" \
  --fee 200000 \
  --out-file tx.raw

# 7. Signer la transaction
cotscli --home ~/my-project transaction sign \
  --tx-file tx.raw \
  --signing-key-file ~/my-project/keys/alice.skey \
  --out-file tx.signed

# 8. Valider la transaction
cotscli --home ~/my-project transaction validate --tx-file tx.signed --db-file demo.db

# 9. Vérifier les UTXO
cotscli --home ~/my-project query utxo --address $(cat alice.addr) --testnet-magic 1 --db-file demo.db
```
