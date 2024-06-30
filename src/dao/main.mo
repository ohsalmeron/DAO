import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Array "mo:base/Array";
import Types "types";
import Buffer "mo:base/Buffer";
import Time "mo:base/Time";
import HashMap "mo:base/HashMap";
import Hash "mo:base/Hash";
import Nat32 "mo:base/Nat32";
import Iter "mo:base/Iter";
import Int "mo:base/Int";

actor DAO {
    type Result<A, B> = Result.Result<A, B>;
    type Member = Types.Member;
    type ProposalContent = Types.ProposalContent;
    type ProposalId = Types.ProposalId;
    type Proposal = Types.Proposal;
    type Vote = Types.Vote;
    type HttpRequest = Types.HttpRequest;
    type HttpResponse = Types.HttpResponse;

    stable let initialMentorPrincipal: Principal = Principal.fromText("nkqop-siaaa-aaaaj-qa3qq-cai");
    stable let canisterIdWebpage: Principal = Principal.fromText("ysmdh-qyaaa-aaaab-qacga-cai");
    stable var manifesto = "Your manifesto";
    stable let name = "Your DAO";
    stable var goals: [Text] = [];

    public shared func addInitialMentor() : async Result<(), Text> {
        switch (members.get(initialMentorPrincipal)) {
            case (null) {
                let initialMentor = {
                    name = "motoko_bootcamp";
                    role = #Mentor;
                };
                members.put(initialMentorPrincipal, initialMentor);
                return #ok(());
            };
            case (?_) {
                return #err("Initial mentor already exists");
            };
        }
    };

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

    public query func getName(): async Text {
        return name;
    };

    public query func getManifesto(): async Text {
        return manifesto;
    };

    public query func getGoals(): async [Text] {
        return goals;
    };

    public shared ({ caller }) func registerMember(member: Member): async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                members.put(caller, { name = member.name; role = #Student });
                let mintResult = await faucet.mint(caller, 10);
                switch (mintResult) {
                    case (#err(errMsg)) {
                        ignore members.remove(caller);
                        return #err(errMsg);
                    };
                    case (#ok(())) {
                        return #ok(());
                    };
                };
            };
            case (?_) {
                return #err("Member already exists");
            };
        }
    };

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
                members.put(student, { name = stud.name; role = #Graduate });
                return #ok(());
            };
            case (null) {
                return #err("Student does not exist");
            };
        }
    };

    public shared ({ caller }) func createProposal(content: ProposalContent): async Result<ProposalId, Text> {
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

        let balance = await faucet.balanceOf(caller);
        if (balance < 1) {
            return #err("Insufficient balance to create a proposal");
        };

        let burnResult = await faucet.burn(caller, 1);
        switch (burnResult) {
            case (#err(errMsg)) {
                return #err(errMsg);
            };
            case (#ok(())) {};
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

    public query func getAllProposals(): async [Proposal] {
        return Iter.toArray(proposals.vals());
    };

    public shared ({ caller }) func voteProposal(proposalId: ProposalId, vote: Vote): async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("The caller is not a member - cannot vote on the proposal");
            };
            case (?member) {
                switch (proposals.get(proposalId)) {
                    case (null) {
                        return #err("The proposal does not exist");
                    };
                    case (?proposal) {
                        if (proposal.status != #Open) {
                            return #err("The proposal is not open for voting");
                        };
                        if (_hasVoted(proposal, caller)) {
                            return #err("The caller has already voted on this proposal");
                        };

                        let balance = await faucet.balanceOf(caller);
                        if (balance == 0) {
                            return #err("Insufficient balance to vote on the proposal");
                        };

                        let multiplierVote: Nat = switch (member.role) {
                            case (#Student) { 0 };
                            case (#Graduate) { balance };
                            case (#Mentor) { balance * 5 };
                        };

                        let newVote: Vote = {
                            member = caller;
                            votingPower = multiplierVote;
                            yesOrNo = vote.yesOrNo;
                        };

                        let newVoteScore: Int = if (vote.yesOrNo) {
                            proposal.voteScore + Int.abs(multiplierVote)
                        } else {
                            proposal.voteScore - Int.abs(multiplierVote)
                        };

                        var newExecuted: ?Time.Time = null;
                        let newVotes = Buffer.fromArray<Vote>(proposal.votes);
                        newVotes.add(newVote);
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
                let buffer = Buffer.Buffer<Text>(goals.size() + 1);
                for (goal in goals.vals()) {
                    buffer.add(goal);
                };
                buffer.add(newGoal);
                goals := Buffer.toArray(buffer);
            };
            case (#AddMentor(newMentor)) {
                switch (members.get(newMentor)) {
                    case (?grad) {
                        if (grad.role == #Graduate) {
                            members.put(newMentor, { name = grad.name; role = #Mentor });
                        };
                    };
                    case (null) {};
                };
            };
        };
        return;
    };

    public query func getIdWebpage(): async Principal {
        return canisterIdWebpage;
    };
};
