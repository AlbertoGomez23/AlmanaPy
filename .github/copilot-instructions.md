# GitHub Copilot Instructions for Almanaque Nautico ROA

This document provides context and guidelines for AI agents working on the `almanaque_nautico_roa` project.

## 🏗 Project Architecture & "Big Picture"

This project is a **migration and modernization** of the Spanish Nautical Almanac generation software from **Fortran 77** (`legacy/`) to **Python 3.12+** (`modern/`).

- **Core Domain**: Astronomical calculations (ephemerides, star positions, moon phases) for navigation.
- **Modern Stack**:
  - **Engine**: `skyfield` and `jplephem` for high-precision astronomy (replacing raw Fortran math).
  - **Data**: Uses NASA JPL SPICE kernels (specifically `de440.bsp`) for planetary positions.
  - **CLI**: `click` for the command-line interface.
- **Directory Structure**:
  - `modern/src/`: Python source code. Subdirectories (`estrellas`, `fase_luna`) mirror the `legacy` folder structure to facilitate logic mapping.
  - `modern/app.py`: Main entry point for the CLI.
  - `legacy/`: Reference Fortran code. **Do not modify** unless debugging the original logic.
  - `data/`: Generated output files and input data.

## 🚀 Critical Workflows

### Running the Application
The application is CLI-based. Run it from the workspace root:
```bash
python modern/app.py --year 2026
```

### Development Environment
- **Dev Container**: The project is designed to run in a Dev Container. Dependencies are pre-installed.
- **Dependencies**: Managed via `requirements.txt` and `pyproject.toml`.
  - Key libs: `skyfield`, `jplephem`, `numpy`.

### Testing
- Tests are located in `modern/src/tests/`.
- Currently, the project relies on comparing outputs against the legacy Fortran system or known benchmarks.

## 📏 Conventions & Patterns

### 1. Documentation Standard
**Strictly follow** the project's function header convention for all new or migrated functions. This style is adapted from the legacy codebase to ensure clarity on scientific preconditions.

```python
def function_name(param1, param2):
    """
    CABECERA:       function_name(param1, param2)
    DESCRIPCIÓN:    Brief description of what the function calculates.
    
    PRECONDICIÓN:   - param1: Description (units, range).
                    - param2: Description.
    
    POSTCONDICIÓN:  Return value description (units, coordinate system).
                    Notes on side effects or specific algorithms used.
    """
    # Implementation...
```

### 2. Migration Strategy: "Modern Implementation, Legacy Interface"
- **Logic**: Do **not** blindly translate Fortran syntax to Python. Use `skyfield` and `numpy` for vector math and astronomical algorithms.
- **Naming**: While Pythonic `snake_case` is preferred for new internal logic, some core scientific functions retain their Legacy names (e.g., `pleph`, `GeoDista`) to maintain conceptual mapping with the original scientific documentation.
- **Singleton Pattern**: Heavy resources (like loading `de440.bsp`) must use a Singleton/Lazy Loading pattern to avoid performance hits. See `modern/src/utils/read_de440.py` for the canonical example.

### 3. Path Handling
- Always use `pathlib.Path`.
- Resolve paths relative to `__file__` to ensure the code runs correctly regardless of the working directory.
- Example: `BASE_DIR = Path(__file__).resolve().parent.parent`

## 🔗 Integration & Dependencies

- **SPICE Kernels**: The system relies on `modern/src/data/de440.bsp`. If missing, `skyfield` may attempt to download it, but the production workflow expects it to be present.
- **Legacy Interop**: The `pleph` function in `read_de440.py` specifically handles `target=14` to return nutation angles, preserving a specific behavior from the old Fortran `PLEPH` subroutine. Respect these "magic numbers" when interfacing with ported logic.
