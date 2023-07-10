# Liquidity Pools

Liquidity pools, typically implemented as blockchain smart contracts, let traders exchange tokens directly between peers. For such purpose, a certain liquidity between the two types of tokens being exchanged must be directly available to traders. This is comes from the liquidity providers, who are paid liquidity provider tokens which make them earn rewards. The exchange rates between the pair of tokens is algorithmically determined from the available liquidity, which explains why liquidity pools are also known as [automated market makers](https://github.com/runtimeverification/verified-smart-contracts/blob/c40c98d6ae35148b76742aaaa29e6eaa405b2f93/uniswap/x-y-k.pdf).

When taking the form of a blockchain smart contract, a liquidity pool does not need a centralized entity of any sort to regulate the exchanges between traders, nor the policies by which liquidity providers are allowed to offer their tokens as liquidity, or redeem them. It is all simply taken care of at the algorithmic level, or by the fact that it runs on a descentralized network, which is highly valuable and desirable over any other centralized alternative.

However, the value of liquidity pools does not lie on its descentralized aspect alone. For instance, the very possibility of offering assets in exchange for interest rates, subject to any particular set of business logic formulae, is a financial application worth having. And so is the possibility of exchanging assets directly with peers, specially when done at an open market's rate. From this viewpoint, one could dare to claim that running a liquidity pool on a blockchain mainly adds an extra layer of security, for liquidity providers and traders, who do not need to trust a third-party running the business.

And that is exactly what this application is meant to provide: all the functionalities expected from a liquidity pool, _save for descentralization_. That is, the application is implemented as a centralized entity accepting assets from liquidity providers, and offering them the possibility to provide said assets as liquidity for traders to exchange with. Both the rewards that liquidity providers earn for their l,oans and the rate at which traders exchange, are still calculated algorithmically, albeit so by the centralized entity.

### Features ###

- Web server API fully implemented using the Servant Haskell library.
- A single command-line application for liquidity providers and traders alike.
- Connection to PostgreSQL for storing all the bank accounts and the actively running liquidity pools.
- Endpoints (refer to **src/Types** for a clearer view on the data types involved in these endpoints):
    - **getPools** (GET): retrieve the list of active liquidity pools.
    - **subscribe** (GET): request the server a new password, used to access the newly created bank account.
    - **getAccount**: provide a password, then retrieve the associated account.
    - **addFunds**: provide a password and an asset (kind and number), then add the funds to the associated account.
    - **rmFunds**: provide a password and an asset, then remove the funds accordingly (if the assets indeed exist).
    - **createPool**: provide a password and a pair of assets, create a new liquidity pool with that liquidity (if the user currently possesses the assets, and teh liquidity pool does not yet exist).
    - **addLiquidity**: provide a password and a pair of assets, then add that liquidity to the corresponding liquidity pool (if it exists).
    - **rmLiquidity**: provide a password, a pool ID and the number of liquidity tokens to burn, then burn the latter, remove the corresponding liquidity from the corresponding pool, and pay it back to the liquidity provider.
    - **swap**: provide a password, an asset and a pool ID, then remove the asset from the associated account (if available), exchange it at the corresponding pool, remove the asset of the other kind from the pool, and pay it to the associated account.
