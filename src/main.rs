use std::{collections::BTreeSet, collections::HashSet, usize};

fn main() {
    let f: LogicFunc = vec![
        logic_expr! { Literal::False, Literal::True, Literal::False, Literal::False },
        logic_expr! { Literal::True, Literal::False, Literal::False, Literal::False },
        logic_expr! { Literal::True, Literal::False, Literal::False, Literal::True },
        logic_expr! { Literal::True, Literal::False, Literal::True, Literal::False },
        logic_expr! { Literal::True, Literal::True, Literal::False, Literal::False },
        logic_expr! { Literal::True, Literal::False, Literal::True, Literal::True },
        logic_expr! { Literal::True, Literal::True, Literal::True, Literal::False },
        logic_expr! { Literal::True, Literal::True, Literal::True, Literal::True },
    ];
    let mut qm = QuineMcClusley::new_enumerate(f);
    for (w, exprs) in qm.same_wight_exprs.iter().enumerate() {
        println!("{}:{:?}", w, exprs);
    }
    println!("----------------------------------------");
    let qm = qm.most_simplify();
    for (w, exprs) in qm.same_wight_exprs.iter().enumerate() {
        println!("{}:{:?}", w, exprs);
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
enum Literal {
    True,
    False,
    None,
}

#[derive(Debug, Default, PartialEq, Eq, Clone, Hash)]
struct LogicExpr {
    pub is_dontcare: bool,
    pub labels: Vec<usize>,
    pub logic_expr: Vec<Literal>,
}

#[macro_export]
macro_rules! logic_expr {
    ( $( $x:expr ),* ) => {
        LogicExpr {
            is_dontcare: false,
            logic_expr: {
                let mut temp_vec = Vec::new();
                $(temp_vec.push($x);)*
                temp_vec
            },
            labels: vec![]
        }
    }
}

impl LogicExpr {
    pub fn literal_label(&self) -> usize {
        (0..self.logic_expr.len())
            .map(|i| {
                if self.logic_expr[i] == Literal::True {
                    1 << (self.logic_expr.len() - i - 1)
                } else {
                    0
                }
            })
            .sum()
    }

    pub fn hamming_wight(&self) -> usize {
        self.logic_expr
            .iter()
            .filter(|x| **x == Literal::True)
            .count()
    }

    pub fn hamming_distance(&self, other: &LogicExpr) -> usize {
        assert_eq!(self.logic_expr.len(), other.logic_expr.len());
        self.logic_expr
            .iter()
            .zip(other.logic_expr.iter())
            .filter(|(a, b)| **a != **b)
            .count()
    }

    pub fn merge(&self, other: &LogicExpr) -> LogicExpr {
        assert_eq!(self.hamming_distance(other), 1);
        assert_eq!(self.logic_expr.len(), other.logic_expr.len());
        let len = self.logic_expr.len();
        let mut res = LogicExpr {
            is_dontcare: false,
            labels: vec![],
            logic_expr: vec![Literal::None; len],
        };
        let (a, b) = (&self.logic_expr, &other.logic_expr);
        for i in 0..len {
            res.logic_expr[i] = if a[i] == b[i] { a[i] } else { Literal::None };
        }
        res
    }
}

type LogicFunc = Vec<LogicExpr>;
type SameWightExpr = Vec<LogicExpr>;
#[derive(Debug, Default, PartialEq, Eq, Clone, Hash)]
struct QuineMcClusley {
    pub same_wight_exprs: Vec<SameWightExpr>,
}

impl QuineMcClusley {
    pub fn new_enumerate(logic_func: LogicFunc) -> QuineMcClusley {
        let max_wight = logic_func[0].logic_expr.len() + 1;
        let mut res = vec![SameWightExpr::new(); max_wight];
        for logic_expr in logic_func {
            let wight = logic_expr.hamming_wight();
            let logic_expr = LogicExpr {
                is_dontcare: false,
                labels: vec![logic_expr.literal_label()],
                logic_expr: logic_expr.logic_expr,
            };
            res[wight].push(logic_expr);
        }
        QuineMcClusley {
            same_wight_exprs: res,
        }
    }

    pub fn enumerate(logic_func: LogicFunc) -> QuineMcClusley {
        let max_wight = logic_func[0].logic_expr.len() + 1;
        let mut res = vec![SameWightExpr::new(); max_wight];
        for logic_expr in logic_func {
            let wight = logic_expr.hamming_wight();
            res[wight].push(logic_expr);
        }
        QuineMcClusley {
            same_wight_exprs: res,
        }
    }

    pub fn simplify(&mut self) -> QuineMcClusley {
        let mut new_func = LogicFunc::new();
        let len = self.same_wight_exprs.len();
        for i in 0..len - 1 {
            for swe1 in &self.same_wight_exprs[i] {
                for swe2 in &self.same_wight_exprs[i + 1] {
                    if swe1.hamming_distance(swe2) == 1 {
                        let expr = swe1.merge(swe2);
                        let mut labels = BTreeSet::<usize>::new();
                        for l in &swe1.labels {
                            labels.insert(*l);
                        }
                        for l in &swe2.labels {
                            labels.insert(*l);
                        }
                        let labels = labels.into_iter().collect::<Vec<_>>();
                        let expr = LogicExpr {
                            is_dontcare: false,
                            labels: labels,
                            logic_expr: expr.logic_expr,
                        };
                        new_func.push(expr);
                    }
                }
            }
        }
        let new_func = new_func
            .into_iter()
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();
        QuineMcClusley::enumerate(new_func)
    }

    pub fn most_simplify(&mut self) -> QuineMcClusley {
        let mut qm = self.simplify();
        loop {
            qm = qm.simplify();
            if self.is_most_simplify() {
                return qm;
            }
        }
    }

    pub fn is_most_simplify(&self) -> bool {
        (0..self.same_wight_exprs.len() - 1).any(|i| {
            self.same_wight_exprs[i].iter().any(|a| {
                self.same_wight_exprs[i + 1]
                    .iter()
                    .any(|b| a.hamming_distance(b) == 1)
            })
        })
    }
}
