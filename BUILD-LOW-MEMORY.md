# Building Egison on Low Memory Systems

## Problem
When building Egison on systems with 8GB or less of RAM, GHC may exhaust available memory and cause build failures.

## Solutions

### 1. Using cabal.project.local (Recommended)
A `cabal.project.local` file has been created with the following configurations:
- Limits parallel build jobs to 1
- Restricts GHC heap size to 3GB
- Reduces optimization level to -O1

**Build command:**
```bash
cabal clean
cabal build
```

### 2. For Even More Memory Savings
Use the development version with optimizations disabled:
```bash
cp cabal.project.local.dev cabal.project.local
cabal clean
cabal build
```

### 3. Using Stack
Similar settings have been added to `stack.yaml`:
```bash
stack clean
stack build
```

### 4. Additional System-Level Strategies

#### Check and Manage Swap Space (macOS)
```bash
# Check current swap usage
sysctl vm.swapusage

# Close unnecessary applications if needed
```

#### Temporary Memory Release
```bash
# Clear memory cache (macOS)
sudo purge
```

### 5. Building Specific Components Only
```bash
# Build library only
cabal build egison:lib

# Build without tests
cabal build --disable-tests
```

## Configuration Details

### GHC Options
- `+RTS -M3G -RTS`: Limits maximum heap size to 3GB
- `-A128M`: Sets allocation area to 128MB
- `-O1`: Moderate optimization (less memory than -O3)
- `-O0`: No optimization (for development, lowest memory usage)
- `-fno-specialise`: Disables function specialization
- `-fmax-simplifier-iterations=2`: Limits optimization iteration count

### Jobs Setting
- `jobs: 1`: Compiles only one module at a time (disables parallel compilation)

## Troubleshooting

### If You Still Experience Out of Memory Errors
1. Use `cabal.project.local.dev` to completely disable optimizations
2. Further restrict memory with `-M2G` or `-M1G`
3. Close all other applications before building

### If Build is Too Slow
- Adjust optimization level (-O0 < -O1 < -O2 < -O3)
- Use -O3 only for release builds, -O1 for development

### Common Error Messages
```
GHC ran out of memory while compiling...
Heap exhausted...
```
If you see these errors, apply the strategies in this document.
