# **COMPRESSION HASKELL**
## *Méthodes de Compression*

## Table des Matières
- [Présentation](#présentation)
- [Installation](#installation)
- [Utilisation](#utilisation)
- [Méthodes de Compression](#méthodes-de-compression)

### Présentation
Ce projet Haskell offre une collection d'algorithmes de compression de texte sans perte, incluant RLE, LZ78, LZW, Huffman et Shannon-Fano. Ces méthodes exploitent les redondances dans les données textuelles pour réduire la taille des fichiers.

### Installation
Pour exécuter les outils de compression, suivez les instructions ci-dessous :

## Étape 1 : Cloner le dépôt
1. Ouvrez votre terminal.
2. Naviguez vers le répertoire de votre choix.
3. Clonez le dépôt avec la commande : git clone [\(https://github.com/ilyazooo/compressHaskell.git)](https://github.com/ilyazooo/compressHaskell.git)

## Étape 2 : Installer Haskell et les dépendances
1. Assurez-vous que [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) est installé sur votre machine.
2. Dans le répertoire du projet, exécutez : stack build


### Utilisation
Placez les fichiers textuels à compresser dans le dossier `txt`. Les scripts de compression traiteront ces fichiers et généreront les versions compressées et décompressées.

### Méthodes de Compression
Le projet implémente les méthodes suivantes :

- **RLE (Run-Length Encoding)** : Compresse les séquences répétées de caractères.
- **LZW (Lempel-Ziv-Welch)** : Utilise un dictionnaire pour remplacer des séquences de données répétées.
- **LZ78 (Lempel-Ziv 78)** : Construit un dictionnaire de séquences de données uniques.
- **Huffman** : Crée un arbre de codage basé sur la fréquence des caractères.
- **Shannon-Fano** : Divise les caractères en deux groupes de fréquence quasi égale pour le codage.

