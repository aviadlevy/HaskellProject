# Use a Haskell base image
FROM haskell:8.10

# Set the working directory in the container
WORKDIR /app

# Copy the project files into the container
COPY . .

# Install any necessary system dependencies
RUN apt-get update && apt-get install -y libncurses5-dev

# Update Cabal and install dependencies
RUN cabal update && cabal install --only-dependencies

# Build the project
RUN cabal build

# Set the command to run the game
CMD ["cabal", "run"]