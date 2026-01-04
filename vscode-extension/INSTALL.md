# Egison VS Code Extension - Installation Guide

## Installation Methods

### Method 1: Direct Installation (Development Version)

#### For Cursor (Recommended)

1. Copy the `vscode-extension` directory to Cursor's extensions folder:

   **macOS/Linux:**
   ```bash
   cd /Users/egisatoshi/egison
   cp -r vscode-extension ~/.cursor/extensions/egison-language-support
   ```

   **Windows (PowerShell):**
   ```powershell
   Copy-Item -Recurse vscode-extension $env:USERPROFILE\.cursor\extensions\egison-language-support
   ```

2. Restart Cursor completely (Quit with ⌘Q/Ctrl+Q and restart)

3. Open any `.egi` file, and Egison syntax highlighting will be automatically applied

#### For VS Code

1. Copy the `vscode-extension` directory to VS Code's extensions folder:

   **macOS/Linux:**
   ```bash
   cd /Users/egisatoshi/egison
   cp -r vscode-extension ~/.vscode/extensions/egison-language-support
   ```

   **Windows (PowerShell):**
   ```powershell
   Copy-Item -Recurse vscode-extension $env:USERPROFILE\.vscode\extensions\egison-language-support
   ```

2. Restart VS Code

3. Open any `.egi` file, and Egison syntax highlighting will be automatically applied

### Method 2: Install from VSIX Package

For a more formal installation, you can create a VSIX package.

1. Install `vsce` (first time only):
   ```bash
   npm install -g @vscode/vsce
   ```

2. Navigate to the vscode-extension directory:
   ```bash
   cd vscode-extension
   ```

3. Create a VSIX package:
   ```bash
   vsce package
   ```

4. Open the Extensions view in Cursor/VS Code (⇧⌘X / Ctrl+Shift+X)

5. Click the "..." menu at the top of the Extensions view

6. Select "Install from VSIX..."

7. Select the generated `.vsix` file

## Verification

1. Open a `.egi` file in Cursor/VS Code
2. Check that the language shows "Egison" in the status bar (bottom right)
3. Verify that keywords, comments, strings, etc. are highlighted

## Test with Sample Files

You can test the extension with sample files in the project:

```bash
# Open in Cursor
cursor sample/poker-hands.egi
# or
cursor test/sample/type/riemann-curvature-tensor-of-S2.egi
```

## Troubleshooting

### Syntax highlighting not applied

1. Completely restart Cursor/VS Code
2. Open Command Palette (⌘⇧P / Ctrl+Shift+P) and run "Developer: Reload Window"
3. Manually set the language mode:
   - Open Command Palette and select "Change Language Mode"
   - Select "Egison"

### Extension not recognized

Check the extension directory structure:

**For Cursor:**
```
~/.cursor/extensions/egison-language-support/
├── package.json
├── language-configuration.json
├── syntaxes/
│   └── egison.tmLanguage.json
└── README.md
```

**For VS Code:**
```
~/.vscode/extensions/egison-language-support/
├── package.json
├── language-configuration.json
├── syntaxes/
│   └── egison.tmLanguage.json
└── README.md
```

## Updating

To update the extension, overwrite the files and restart Cursor/VS Code.

## Uninstallation

Find "Egison Language Support" in the Extensions view and click the uninstall button.

Or directly delete the extension directory:

**For Cursor:**
```bash
rm -rf ~/.cursor/extensions/egison-language-support
```

**For VS Code:**
```bash
rm -rf ~/.vscode/extensions/egison-language-support
```

## Quick Install Script

You can also use this quick installation script:

**For Cursor (macOS/Linux):**
```bash
cd /path/to/egison
cp -r vscode-extension ~/.cursor/extensions/egison-language-support && echo "✅ Installed! Please restart Cursor."
```

**For VS Code (macOS/Linux):**
```bash
cd /path/to/egison
cp -r vscode-extension ~/.vscode/extensions/egison-language-support && echo "✅ Installed! Please restart VS Code."
```

## Language Files

- **English**: `INSTALL.md` (this file)
- **Japanese**: `INSTALL.ja.md`
