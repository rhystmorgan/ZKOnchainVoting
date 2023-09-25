import Image from "next/image";
import { useState } from "react";
import Router from "next/router";
import React, { useEffect, useContext } from "react";
import { ToastContext } from "@/components/utils/Toast";
import initLucid from "@/functions/lucid";
import { Blockfrost, Lucid, Network, Assets, toUnit, Constr, Data } from "lucid-cardano";
import { useStoreActions, useStoreState } from "@/functions/store";
import { generateUniqueId } from "@/functions/utils";
// import { threadMintingPolicy, lockingScript, treasuryScript } from "@/functions/scripts";
// import { DatumV1, MetadataMap, OracleData } from "@/functions/datum";

// Submit Data to Oracle
const ZKVoting = () => {
	const walletStore = useStoreState((state) => state.wallet);
	const [lucid, setLucid] = useState<Lucid | null>(null);
	const [TxHash, setTxHash] = useState("None");

	const { addToast } = useContext(ToastContext)!;

	function createToaster(mes: string, type: string) {
		addToast({ id: generateUniqueId(), message: mes, color: type });
	}

	useEffect(() => {
		if (!lucid && walletStore.connected) {
			initLucid(walletStore.name).then((Lucid: Lucid) => {
				setLucid(Lucid);
			});
		}
	}, [lucid]);

	const mintCredential = async () => {
		try {
			if (!lucid) {
				throw Error("Lucid not instantiated");
			}

      const voterRegistry = contracts.registry.script
      const voterMintingPolicy = contracts.mintingPolicy.script



      const utxos = await lucid.wallet.getUtxos()
      const voterAddress = await lucid.wallet.address
      const voterCS = lucid.utils.mintingPolicyToId(voterMintingPolicy)
      const voterTN = Date.now()
      

			const txHash = await lucid.newTx()
        .collectFrom([utxos[0]])
        .mintAssets({
          [toUnit(voterCS, voterTN, 100)]: BigInt(1),
          [toUnit(voterCS, voterTN, 222)]: BigInt(1)
        }, Data.to(BigInt(0)))
        .payToContract(voterRegistryAddress, metadataDatum, {[toUnit(preMintingCS, tokenName, 100)]: BigInt(1)})
        .payToAddress(voterAddress, metadataDatum, {[toUnit(preMintingCS, tokenName, 222)]: BigInt(1)})
        .attachMintingPolicy(preMinting)
        .complete()
        .then((tx) => tx.sign().complete())
        .then((tx) => tx.submit())
			console.log(txHash)
			return txHash
		} catch (e: any) {
			console.log(e);
			createToaster(e.toString(), "alert");
		}
	}

	const verifyCredential = async () => {
		try {
			if (!lucid) {
				throw Error("Lucid not instantiated");
			}
    } catch (e: any) {
      console.log(e);
      createToaster(e.toString(), "alert")
    }

    const txHAsh = await.lucid.newTx()
      .collectFrom()
      .
  }

	const closeData = async () => {
		try {
			if (!lucid) {
				throw Error("Lucid not instantiated");
			}

		} catch (e: any) {
			console.log(e);
			createToaster(e.toString(), "alert");
		}
	}


	return (
		<div>
			{" "}
			{lucid && walletStore.connected ? (
				<div className="flex-column justify-center text-center ">
					<div className="text-2xl my-20">
						Your Test Transaction buttons below
					</div>
					<div className="text-lg my-6">
						Last Transaction Tx Hash: {TxHash}{" "}
					</div>
					<div className="flex items-center">
						<h2 className="">For Script 1</h2>
						<button className="normal-button px-12" onClick={mintCredential}>
							MintCredential
						</button>
						<button className="normal-button px-12" onClick={verifyCredential}>
							VerifyCredential
						</button>
						<button className="normal-button px-12" onClick={VoteTx}>
							VoteTx
						</button>
            <button className="normal-button px-12" onClick={BurnCredential}>
							BurnCredential
						</button>
            <button className="normal-button px-12" onClick={extra}>
							extra
						</button>
					</div>
				</div>
			) : (
				<div>
					<div className="text-2xl my-20">
						{" "}
						Connect a Wallet to Initiate TXs
					</div>
				</div>
			)}
		</div>
	);
};

export default ZKVoting;
