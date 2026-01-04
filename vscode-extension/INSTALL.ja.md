# Egison VS Code Extension - インストールガイド

**Language**: [English](INSTALL.md) | 日本語

## インストール方法

### 方法1: 開発版として直接インストール

#### Cursor の場合（推奨）

1. この `vscode-extension` ディレクトリを Cursor の拡張機能フォルダにコピーします：

   **macOS/Linux:**
   ```bash
   cd /Users/egisatoshi/egison
   cp -r vscode-extension ~/.cursor/extensions/egison-language-support
   ```

   **Windows (PowerShell):**
   ```powershell
   Copy-Item -Recurse vscode-extension $env:USERPROFILE\.cursor\extensions\egison-language-support
   ```

2. Cursor を再起動します（⌘Q で完全に終了してから再起動）

3. `.egi` ファイルを開くと、自動的に Egison のシンタックスハイライトが適用されます

#### VS Code の場合

1. この `vscode-extension` ディレクトリを VS Code の拡張機能フォルダにコピーします：

   **macOS/Linux:**
   ```bash
   cd /Users/egisatoshi/egison
   cp -r vscode-extension ~/.vscode/extensions/egison-language-support
   ```

   **Windows (PowerShell):**
   ```powershell
   Copy-Item -Recurse vscode-extension $env:USERPROFILE\.vscode\extensions\egison-language-support
   ```

2. VS Code を再起動します

3. `.egi` ファイルを開くと、自動的に Egison のシンタックスハイライトが適用されます

### 方法2: VSIXパッケージを作成してインストール

より正式なインストール方法として、VSIXパッケージを作成できます。

1. `vsce` をインストール（初回のみ）:
   ```bash
   npm install -g @vscode/vsce
   ```

2. vscode-extension ディレクトリに移動:
   ```bash
   cd vscode-extension
   ```

3. VSIXパッケージを作成:
   ```bash
   vsce package
   ```

4. VS Code で Extensions ビューを開く (⇧⌘X / Ctrl+Shift+X)

5. 右上の "..." メニューから "Install from VSIX..." を選択

6. 生成された `.vsix` ファイルを選択

## 動作確認

1. VS Code で `.egi` ファイルを開く
2. 右下のステータスバーで言語が "Egison" になっていることを確認
3. キーワード、コメント、文字列などがハイライトされることを確認

## サンプルファイルでテスト

プロジェクト内のサンプルファイルで動作確認できます：

```bash
code sample/poker-hands.egi
code sample/primes.egi
code sample/math/geometry/riemann-curvature-tensor-of-S2.egi
```

## トラブルシューティング

### シンタックスハイライトが適用されない場合

1. VS Code を完全に再起動してください
2. コマンドパレット (⌘⇧P / Ctrl+Shift+P) を開いて "Developer: Reload Window" を実行
3. ファイルの言語モードを手動で設定：
   - コマンドパレットで "Change Language Mode" を選択
   - "Egison" を選択

### 拡張機能が認識されない場合

拡張機能のディレクトリ構造を確認してください：

**Cursor の場合:**
```
~/.cursor/extensions/egison-language-support/
├── package.json
├── language-configuration.json
├── syntaxes/
│   └── egison.tmLanguage.json
└── README.md
```

**VS Code の場合:**
```
~/.vscode/extensions/egison-language-support/
├── package.json
├── language-configuration.json
├── syntaxes/
│   └── egison.tmLanguage.json
└── README.md
```

## 更新

拡張機能を更新する場合は、ファイルを上書きしてVS Codeを再起動してください。

## アンインストール

Cursor/VS Codeの拡張機能ビューから "Egison Language Support" を見つけて、アンインストールボタンをクリックします。

または、拡張機能のディレクトリを直接削除します：

**Cursor の場合:**
```bash
rm -rf ~/.cursor/extensions/egison-language-support
```

**VS Code の場合:**
```bash
rm -rf ~/.vscode/extensions/egison-language-support
```

