import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Array "mo:base/Array";
import Types "types";
import Buffer "mo:base/Buffer";
import Time "mo:base/Time";
import HashMap "mo:base/HashMap";
import Hash "mo:base/Hash";
import Nat32 "mo:base/Nat32";
import Iter "mo:base/Iter";
import Option "mo:base/Option";

actor DAO {
    type Result<A, B> = Result.Result<A, B>;
    type Member = Types.Member;
    type ProposalContent = Types.ProposalContent;
    type ProposalId = Types.ProposalId;
    type Proposal = Types.Proposal;
    type Vote = Types.Vote;
    type HttpRequest = Types.HttpRequest;
    type HttpResponse = Types.HttpResponse;

    stable let canisterIdWebpage: Principal = Principal.fromText("aaaaa-aa");
    stable var manifesto = "Your manifesto";
    stable let name = "Your DAO";
    stable var goals: [Text] = [];

    // Custom hash function for Nat values
    func customHashNat(x: Nat): Hash.Hash {
        var hash: Nat32 = 0;
        var value = x;
        while (value != 0) {
            let byte = Nat32.fromNat(value % 256);
            hash := hash + byte;
            hash := hash + (hash << 10);
            hash := hash ^ (hash >> 6);
            value := value / 256;
        };
        hash := hash + (hash << 3);
        hash := hash ^ (hash >> 11);
        hash := hash + (hash << 15);
        return hash;
    };

    let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);
    let proposals = HashMap.HashMap<ProposalId, Proposal>(0, Nat.equal, customHashNat);
    stable var nextProposalId: ProposalId = 0;

    // The faucet canister
    let faucet = actor("jaamb-mqaaa-aaaaj-qa3ka-cai") : actor {
        mint: shared (to: Principal, amount: Nat) -> async Result<(), Text>;
        burn: shared (from: Principal, amount: Nat) -> async Result<(), Text>;
        balanceOf: shared (account: Principal) -> async Nat;
        balanceOfArray: shared (accounts: [Principal]) -> async [Nat];
        tokenName: shared () -> async Text;
        tokenSymbol: shared () -> async Text;
        totalSupply: shared () -> async Nat;
        transfer: shared (from: Principal, to: Principal, amount: Nat) -> async Result<(), Text>;
    };

    // Returns the name of the DAO
    public query func getName(): async Text {
        return name;
    };

    // Returns the manifesto of the DAO
    public query func getManifesto(): async Text {
        return manifesto;
    };

    // Returns the goals of the DAO
    public query func getGoals(): async [Text] {
        return goals;
    };

    // Register a new member in the DAO with the given name and principal of the caller
    // Airdrop 10 MBC tokens to the new member
    // New members are always Student
    // Returns an error if the member already exists
    public shared ({ caller }) func registerMember(member: Member): async Result<(), Text> {
        // Check if the member already exists
        switch (members.get(caller)) {
            case (null) {
                // Add the new member
                members.put(caller, { name = member.name; role = #Student });
                // Mint tokens for the new member
                let mintResult = await faucet.mint(caller, 10);
                switch (mintResult) {
                    case (#err(errMsg)) {
                        // Rollback the member addition if minting fails
                        ignore members.remove(caller);
                        return #err(errMsg);
                    };
                    case (#ok) {
                        return #ok(());
                    };
                };
            };
            case (?_) {
                return #err("Member already exists");
            };
        }
    };

    // Get the member with the given principal
    // Returns an error if the member does not exist
    public query func getMember(p: Principal): async Result<Member, Text> {
        switch (members.get(p)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                return #ok(member);
            };
        }
    };

    // Graduate the student with the given principal
    // Returns an error if the student does not exist or is not a student
    // Returns an error if the caller is not a mentor
    public shared ({ caller }) func graduate(student: Principal): async Result<(), Text> {
        switch (members.get(caller)) {
            case (?mentor) {
                if (mentor.role != #Mentor) {
                    return #err("Caller is not a mentor");
                };
            };
            case (null) {
                return #err("Caller is not a mentor");
            };
        };

        switch (members.get(student)) {
            case (?stud) {
                if (stud.role != #Student) {
                    return #err("Member is not a student");
                };
                // Update member role to Graduate
                members.put(student, { name = stud.name; role = #Graduate });
                return #ok(());
            };
            case (null) {
                return #err("Student does not exist");
            };
        }
    };

    // Create a new proposal and returns its id
    // Returns an error if the caller is not a mentor or doesn't own at least 1 MBC token
    public shared ({ caller }) func createProposal(content: ProposalContent): async Result<ProposalId, Text> {
        // Check if the caller is a member and a mentor
        switch (members.get(caller)) {
            case (?mentor) {
                if (mentor.role != #Mentor) {
                    return #err("Caller is not a mentor");
                };
            };
            case (null) {
                return #err("Caller is not a mentor");
            };
        };

        // Check the caller's balance
        let balance = await faucet.balanceOf(caller);
        if (balance < 1) {
            return #err("Insufficient balance to create a proposal");
        };

        // Burn 1 token to create a proposal
        let burnResult = await faucet.burn(caller, 1);
        switch (burnResult) {
            case (#err(errMsg)) {
                return #err(errMsg);
            };
            case (#ok) {};
        };

        let proposal = {
            id = nextProposalId;
            content = content;
            creator = caller;
            created = Time.now();
            executed = null;
            votes = [];
            voteScore = 0;
            status = #Open;
        };
        proposals.put(nextProposalId, proposal);
        nextProposalId += 1;
        return #ok(nextProposalId - 1);
    };

    // Get the proposal with the given id
    // Returns an error if the proposal does not exist
    public query func getProposal(id: ProposalId): async Result<Proposal, Text> {
        switch (proposals.get(id)) {
            case (null) {
                return #err("Proposal does not exist");
            };
            case (?proposal) {
                return #ok(proposal);
            };
        }
    };

    // Returns all the proposals
    public query func getAllProposals(): async [Proposal] {
        return Iter.toArray(proposals.vals());
    };

    // Vote for the given proposal
    // Returns an error if the proposal does not exist or the member is not allowed to vote
    public shared ({ caller }) func voteProposal(proposalId: ProposalId, vote: Vote): async Result<(), Text> {
        // Check if the caller is a member of the DAO
        switch (members.get(caller)) {
            case (null) {
                return #err("The caller is not a member - cannot vote on the proposal");
            };
            case (?member) {
                // Check if the proposal exists
                switch (proposals.get(proposalId)) {
                    case (null) {
                        return #err("The proposal does not exist");
                    };
                    case (?proposal) {
                        // Check if the proposal is open for voting
                        if (proposal.status != #Open) {
                            return #err("The proposal is not open for voting");
                        };
                        // Check if the caller has already voted
                        if (_hasVoted(proposal, caller)) {
                            return #err("The caller has already voted on this proposal");
                        };
                        
                        // Check the caller's balance
                        let balance = await faucet.balanceOf(caller);
                        if (balance == 0) {
                            return #err("Insufficient balance to vote on the proposal");
                        };

                        let multiplierVote = switch (vote.yesOrNo) {
                            case (true) { 1 };
                            case (false) { -1 };
                        };
                        let newVoteScore = proposal.voteScore + balance * multiplierVote;
                        var newExecuted: ?Time.Time = null;
                        let newVotes = Buffer.fromArray<Vote>(proposal.votes);
                        let newStatus = if (newVoteScore >= 100) {
                            #Accepted;
                        } else if (newVoteScore <= -100) {
                            #Rejected;
                        } else {
                            #Open;
                        };
                        switch (newStatus) {
                            case (#Accepted) {
                                _executeProposal(proposal.content);
                                newExecuted := ?Time.now();
                            };
                            case (_) {};
                        };
                        let newProposal: Proposal = {
                            id = proposal.id;
                            content = proposal.content;
                            creator = proposal.creator;
                            created = proposal.created;
                            executed = newExecuted;
                            votes = Buffer.toArray(newVotes);
                            voteScore = newVoteScore;
                            status = newStatus;
                        };
                        proposals.put(proposal.id, newProposal);
                        return #ok();
                    };
                };
            };
        };
    };

    func _hasVoted(proposal : Proposal, member : Principal) : Bool {
        return Array.find<Vote>(
            proposal.votes,
            func(vote : Vote) {
                return vote.member == member;
            },
        ) != null;
    };

    func _executeProposal(content : ProposalContent) : () {
        switch (content) {
            case (#ChangeManifesto(newManifesto)) {
                manifesto := newManifesto;
            };
            case (#AddGoal(newGoal)) {
                // Using Buffer to add a new goal
                let buffer = Buffer.Buffer<Text>(goals.size() + 1); // Initialize buffer with extra capacity
                for (goal in goals.vals()) {
                    buffer.add(goal);
                };
                buffer.add(newGoal); // Add the new goal
                goals := Buffer.toArray(buffer); // Convert back to array
            };
        };
        return;
    };

    // Returns the Principal ID of the Webpage canister associated with this DAO canister
    public query func getIdWebpage(): async Principal {
        return canisterIdWebpage;
    };
};
