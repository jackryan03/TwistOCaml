#!/bin/bash

# Define file paths
TIMES_FILE="times.csv"
TURNS_FILE="turns.csv"
EQUIPPED_FILE="equipped.csv"
GALLERY_FILE="gallery.csv"
INVENTORY_FILE="inventory.csv"
PURSE_FILE="purse.csv"

# Reset Times.csv to default values
echo -e "highest,0\nlowest,9999" > "$TIMES_FILE"
echo "Reset $TIMES_FILE to default values."

# Reset Turns.csv to default values
echo -e "highest,0\nlowest,9999" > "$TURNS_FILE"
echo "Reset $TURNS_FILE to default values."

# Reset equipped.csv to default value
echo "1" > "$EQUIPPED_FILE"
echo "Reset $EQUIPPED_FILE to default value."

# Reset gallery.csv to be empty
echo "" > "$GALLERY_FILE"
echo "Reset $GALLERY_FILE to be empty."

# Reset inventory.csv to default value
echo "1" > "$INVENTORY_FILE"
echo "Reset $INVENTORY_FILE to default value."

# Reset purse.csv to default value
echo "0" > "$PURSE_FILE"
echo "Reset $PURSE_FILE to default value."

# Confirmation message
echo "All game files have been reset to their default values."