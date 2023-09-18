# DID Signature & Verification process explained

user token is present in user
s wallet, used for verification and transaction signing.

governing bodies have their own token, that verifiers will search for as proof of valid credentials.

---

user minting process

---

a potential voter will register to vote, usually with their local council.

They will mint a CIP 68 token pair which will be added to the voting registry.

User Mintng -> User Token to their wallet
                Refernece Token with credential to registry

Token info -> 
  - Voting Credential PolicyID.(100)POSIXTime
  - Token Datum {
    Credential: X Voting Crerdential
    Signatories: []
    IssuedOn: POSIXTime
    ValidUntil: POSIXTime
  }

voters will mint tokens for voting and submit proof / id / kyc information to the governing body off chain.

A governing body will verify the information and sign reference token in the locking contract as a confirmation of verified credential.

Token info ->
  - Voting Credential PolicyID.(100)POSIXTime
  - TokenDatum {
    Credential: X Voting Crerdential
    Signatories: [ govPolicyId.(222)POSIXTime ]
    IssuedOn: POSIXTime
    ValidUntil: POSIXTime
  }

only verified token votes will count.

An indexer can check if a token has a certain signature, making sure that they are verified to vote by the appropriate governing body, otherwise their vote will not count.

Transaction

User utxo:
  - user credential token
  - voting choice redeemer

Voting on chain will be done with a registry to enable for easier counting and managing of votes.

A token redeemer
