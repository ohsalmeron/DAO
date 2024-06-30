import Types "types";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";

actor Webpage {

    type Result<A, B> = Result.Result<A, B>;
    type HttpRequest = Types.HttpRequest;
    type HttpResponse = Types.HttpResponse;

    // The manifesto stored in the webpage canister should always be the same as the one stored in the DAO canister
    stable var manifesto: Text = "Let's graduate!";

    // The principal ID of the DAO canister allowed to set the manifesto
    stable let daoCanister: Principal = Principal.fromText("bd3sg-teaaa-aaaaa-qaaba-cai"); // Replace with your actual DAO canister ID

    // The webpage displays the manifesto
    public query func http_request(_request: HttpRequest): async HttpResponse {
        return {
            status_code = 200;
            headers = [];
            body = Text.encodeUtf8(manifesto);
            streaming_strategy = null;
        };
    };

    // This function should only be callable by the DAO canister (no one else should be able to change the manifesto)
    public shared ({ caller }) func setManifesto(newManifesto: Text): async Result<(), Text> {
        if (caller == daoCanister) {
            manifesto := newManifesto;
            return #ok(());
        } else {
            return #err("Unauthorized");
        }
    };
};
