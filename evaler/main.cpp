#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <list>
#include <string>
#include <stack>
#include <algorithm>

using namespace std;

enum class ExpOperator {
    Minus, Plus, Multiplication, Divide,Power, LBracket,RBracket, Number
};

class PriorComp {
    public:
        PriorComp(){}
        bool operator()(pair< ExpOperator,int > &a, pair< ExpOperator,int > &b){
            return a.second < b.second;
        }
};

typedef long double res_type;
typedef list< pair<ExpOperator, res_type> > Expr;
typedef priority_queue< pair< ExpOperator,int >, vector< pair< ExpOperator,int > > , PriorComp > OPS_PRIORITY;

Expr parse(string s) {
    set< char > DIGITS = {'1','2','3','4','5','6','7','8','9','0','.',','};
    Expr res;
    int st = 0;
    while ( static_cast<size_t>(st) < s.size() ){
        int symLen = 1; // Defaut
        if      ( s.at(st) == '(' )
            res.push_back(make_pair(ExpOperator::LBracket, -1.));
        else if ( s.at(st) == ')' )
            res.push_back(make_pair(ExpOperator::RBracket, -1.));
        else if ( s.at(st) == '-' )
            res.push_back(make_pair(ExpOperator::Minus, -1.));
        else if ( s.at(st) == '+' )
            res.push_back(make_pair(ExpOperator::Plus, -1.));
        else if ( s.at(st) == '*' )
            res.push_back(make_pair(ExpOperator::Multiplication, -1.));
        else if ( s.at(st) == '/' )
            res.push_back(make_pair(ExpOperator::Divide, -1.));
        else if ( s.at(st) == '^' )
            res.push_back(make_pair(ExpOperator::Power, -1.));
        else if ( DIGITS.find(s.at(st)) != DIGITS.end() ){
            size_t end;
            res_type cur_num = stold(string(s.begin()+st,s.end()), &end);
            // end is the first position which is not a number
            // end is exactly line size because it's stored from start of number
            symLen = end;
            res.push_back(make_pair(ExpOperator::Number, cur_num));
        }
            
        st += symLen;
    }
    return res;
}

void print(Expr &l) {
    clog << l.size() << " elemets: ";
    for ( auto el: l)
        if ( el.first == ExpOperator::Number )
            clog << el.second << " -> ";
        else
            clog << "op=" << int(el.first) << " -> ";
    clog << endl;
}
// Function recursive and exp is changing!
// First argument can be broken
// Need to call with copy if need to save original
// TITO enabled
pair<res_type,int> evalExpr(Expr &exp, OPS_PRIORITY priors, Expr::iterator it_begin) {
    set< ExpOperator > cur_ops;
    bool brack_mode = false; // Is the first lexem == left bracket "("
    auto it_end = exp.end(); 
    
    if ( it_begin->first == ExpOperator::LBracket )
        brack_mode = true;
    
    for (auto it = it_begin; it != prev(exp.end()); ++it){
        // bad because iterators corrupts
        //        6 * ( 4 + 7 )  after 6 * 11
        // bad_it     ^                    ^
        //     it   ^                    ^
        auto bad_it = next(it); // can be corrupted!
        if ( bad_it->first == ExpOperator::LBracket )
            evalExpr(exp, priors, bad_it);
        else if ( brack_mode && (bad_it->first == ExpOperator::RBracket) )
            it_end = bad_it; // points to right bracket ")"
    }
    
    if ( brack_mode ){
        // removes left bracket "("
        it_begin = exp.erase(it_begin);
    }
    
    do {
        cur_ops.clear();
        auto cur_prior = priors.top().second;
        do {
            cur_ops.insert(priors.top().first);
            priors.pop();
        } while ( !priors.empty() && (priors.top().second == cur_prior) );
        // example  2*3/4^5⋅6 = 0,03515625
        // if powering then iterating in reverse order
        if ( cur_ops.find(ExpOperator::Power) != cur_ops.end() ){
            auto it_rbegin = Expr::reverse_iterator(it_end);
            auto it_rend   = Expr::reverse_iterator(it_begin);
            bool stop_powering = false;
            for (auto it = it_rbegin; (it != it_rend) && !stop_powering; ++it){
                if ( cur_ops.find(it->first) == cur_ops.end() ){
                    continue;
                }
                // it is reversed
                
                res_type t_value;
                t_value = pow(next(it)->second, prev(it)->second);
                advance(it, 2);
                if ( it == it_rend )
                    stop_powering = true;
                exp.insert(it.base(), make_pair(ExpOperator::Number,t_value));
                if ( it == it_rend ){
                    it_begin = exp.erase(it.base(), prev(it, 3).base());
                } else
                    exp.erase(it.base(), prev(it, 3).base()); // first > last because it's reversed iterators
            }
        } else {
            for (auto it = it_begin; it != it_end; ++it){
                if ( cur_ops.find(it->first) == cur_ops.end() )
                    continue;
                bool repl = false;
                res_type t_value;
                if ( it->first == ExpOperator::Multiplication ) {
                    t_value = next(it)->second * prev(it)->second;
                    repl = true;
                } else if ( it->first == ExpOperator::Divide ) {
                    t_value = prev(it)->second / next(it)->second;
                    repl = true;
                } else if ( it->first == ExpOperator::Plus ) {
                    t_value = prev(it)->second + next(it)->second;
                    repl = true;
                } else if ( it->first == ExpOperator::Minus ) {
                    t_value = prev(it)->second - next(it)->second;
                    repl = true;
                }
                if ( repl ) {
                    advance(it, 2);
                    exp.insert(it, make_pair(ExpOperator::Number,t_value));
                    advance(it, -1);
                    if ( prev(it, 3) == it_begin ){
                        it_begin = exp.erase(prev(it, 3), it);
                    } else
                        exp.erase(prev(it, 3), it); // first > last because it's reversed iterators
                }
            }
        }
    } while ( !priors.empty() );
    
    if ( brack_mode ) {
        // removes right bracket ")"
        exp.erase(it_end);
    }
    
    if ( exp.size() == 1 ) {
        return make_pair(exp.begin()->second, 0); // simplification is finished
    } else {
        return make_pair(exp.begin()->second, 1); // simplification isn't finished yet
    }
}

res_type eval(Expr exp){
    OPS_PRIORITY priors; // priorities of operators
    priors.push(make_pair(ExpOperator::Minus, 1));
    priors.push(make_pair(ExpOperator::Plus, 1));
    priors.push(make_pair(ExpOperator::Divide, 2));
    priors.push(make_pair(ExpOperator::Multiplication, 2));
    priors.push(make_pair(ExpOperator::Power, 3));
    pair<res_type,int> res;
    do {
        res = evalExpr(exp, priors, exp.begin());
    } while ( res.second != 0 );
    return res.first;
}

int main(int argc, char **argv) {
    cout << "Enter expression: ";
	string s;
    getline(cin, s);
    auto parsed = parse(s);
    cout << s << " = " << endl << "  " << eval(parsed) << endl;
	return 0;
}
