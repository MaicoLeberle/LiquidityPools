#!/bin/bash

psql -d liquiditypools -a -f clearLiquidityPools.sql
psql -d liquiditypools -a -f liquidityPoolsSchema.sql
