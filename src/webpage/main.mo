import Types "types";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";

actor Webpage {

    type Result<A, B> = Result.Result<A, B>;
    type HttpRequest = Types.HttpRequest;
    type HttpResponse = Types.HttpResponse;

    stable var manifesto: Text = "Let's graduate!";

    stable let daoCanister: Principal = Principal.fromText("bd3sg-teaaa-aaaaa-qaaba-cai");

    public query func http_request(_request: HttpRequest): async HttpResponse {
        return {
            status_code = 200;
            headers = [];
            body = Text.encodeUtf8(manifesto);
            streaming_strategy = null;
        };
    };

    public shared ({ caller }) func setManifesto(newManifesto: Text): async Result<(), Text> {
        if (caller == daoCanister) {
            manifesto := newManifesto;
            return #ok(());
        } else {
            return #err("Unauthorized");
        }
    };
};
