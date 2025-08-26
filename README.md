# ClanWars - On-chain League System

A decentralized league system where gaming clans can register, challenge each other, and climb ranks with automated rewards and governance distribution.

## Features

- Clan registration with membership management
- Challenge system with stake-based matches
- Automated ranking system based on wins/losses
- Season-based reward distribution
- On-chain leaderboards and statistics

## Contract Functions

### Public Functions
- `register-clan` - Register new clan with members list
- `create-challenge` - Create challenge with stake
- `accept-challenge` - Accept pending challenge
- `report-result` - Report match results and update rankings
- `distribute-season-rewards` - Distribute rewards based on performance

### Read-Only Functions
- `get-clan` - Get clan details by ID
- `get-clan-by-name` - Get clan by name
- `get-challenge` - Get challenge details
- `get-leaderboard-position` - Get clan rank points
- `get-season-reward` - Get season reward amount

## Ranking System

- Base rank points: 1000
- Win: +50 points
- Loss: -25 points
- Season rewards based on final rank points

## Usage

1. Register clan with 1 STX fee
2. Create challenges with stake amount
3. Accept challenges to begin matches
4. Report results to update rankings
5. Receive season rewards automatically