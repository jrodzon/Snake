# Snake Game

A classic Snake game implementation in Haskell using the Gloss graphics library.

## Overview

This project implements the classic Snake game where the player controls a snake that moves around the screen, eating food to grow longer. The game ends when the snake hits a wall or itself.

## Features

- Classic Snake gameplay
- Graphics rendering using Gloss
- Random food placement
- Collision detection
- Comprehensive test suite with QuickCheck and HUnit

## Prerequisites

### System Requirements

- **Operating System**: Linux (tested on Fedora 42)
- **GHC**: Version 9.6.6 or compatible
- **Cabal**: Version 3.10.3.0 or compatible

### System Dependencies

The game requires OpenGL libraries for graphics rendering. Install them using your system package manager:

#### Fedora/RHEL/CentOS:
```bash
sudo dnf install -y mesa-libGL-devel freeglut-devel
```

#### Ubuntu/Debian:
```bash
sudo apt-get install -y libgl1-mesa-dev freeglut3-dev
```

#### Arch Linux:
```bash
sudo pacman -S mesa freeglut
```

## Installation

### 1. Clone the Repository
```bash
git clone <repository-url>
cd Snake
```

### 2. Install Haskell Dependencies

Update the Cabal package database:
```bash
cabal update
```

### 3. Build the Project
```bash
cabal build
```

## Running the Game

### Start the Game
```bash
cabal run snake-exe
```

### Game Controls
- **Arrow Keys**: Move the snake (Up, Down, Left, Right)
- **ESC**: Exit the game

### Game Rules
- Control the snake to eat the red food
- Each food eaten makes the snake longer
- Avoid hitting the walls or the snake's own body
- The game ends when collision occurs

## Testing

Run the test suite to verify everything is working correctly:
```bash
cabal test
```

The test suite includes:
- Unit tests for game logic
- QuickCheck property-based tests
- Integration tests

## Development

### Project Structure
```
Snake/
├── app/
│   └── Main.hs              # Application entry point
├── src/
│   ├── SnakeGame.hs         # Main game logic and rendering
│   ├── SnakeGameState.hs    # Game state data types
│   ├── SnakeMoveEngine.hs   # Movement and collision logic
│   └── SnakeRandomGenerator.hs # Random food placement
├── test/
│   ├── QuickCheckTests.hs   # Property-based tests
│   ├── Spec.hs             # Test suite main
│   └── UnitTests.hs        # Unit tests
├── snake.cabal             # Cabal package configuration
├── cabal.project           # Cabal project settings
└── README.md               # This file
```

### Building for Development
```bash
# Clean previous builds
cabal clean

# Build with verbose output
cabal build --verbose

# Build and run tests
cabal test --verbose
```

### Adding Dependencies
To add new dependencies, edit `snake.cabal` and add them to the `build-depends` section:

```cabal
build-depends:
    base >=4.7 && <5
    gloss >=1.11
    random
    your-new-dependency
```

Then run:
```bash
cabal build
```

## Troubleshooting

### Common Issues

#### 1. OpenGL/Graphics Issues
**Error**: `Failed to build OpenGLRaw-3.3.4.1`

**Solution**: Install the required system dependencies:
```bash
# Fedora/RHEL
sudo dnf install -y mesa-libGL-devel freeglut-devel

# Ubuntu/Debian
sudo apt-get install -y libgl1-mesa-dev freeglut3-dev
```

#### 2. Display Issues
**Error**: Game crashes immediately with "You hit the wall"

**Solution**: This is normal behavior if running without a display. The game requires a graphical environment to run properly.

#### 3. Dependency Resolution Issues
**Error**: `Could not resolve dependencies`

**Solution**: Update the package database and try again:
```bash
cabal update
cabal build
```

#### 4. GHC Version Compatibility
**Error**: Version conflicts with base or other core packages

**Solution**: The project is configured to work with GHC 9.6.6. If using a different version, you may need to adjust version constraints in `snake.cabal`.

### Getting Help

If you encounter issues:
1. Check that all system dependencies are installed
2. Ensure you have a compatible GHC version
3. Try cleaning and rebuilding: `cabal clean && cabal build`
4. Check the build logs for detailed error messages

## License

This project is licensed under the BSD3 License - see the LICENSE file for details.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Run the test suite: `cabal test`
6. Submit a pull request

## Acknowledgments

- Built with [Haskell](https://www.haskell.org/)
- Graphics powered by [Gloss](https://hackage.haskell.org/package/gloss)
- Testing with [QuickCheck](https://hackage.haskell.org/package/QuickCheck) and [HUnit](https://hackage.haskell.org/package/HUnit)