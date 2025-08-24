// Prisoner's Dilemma Simulator & Evolutionary Tournament
// Single-file, self-contained C# console application.
// .NET 6+ recommended (top-level statements avoided for compatibility).
//
// FEATURES
// - Horizons: One-shot, Fixed length L, Geometric (indefinite) with continuation probability δ.
// - Noise: none, constant (implementation + observation), variable (per-round random ranges).
// - Strategies (research-grounded):
//   * Tit For Tat (TFT)
//   * Generous Tit For Tat (GTFT)
//   * Contrite Tit For Tat (CTFT)
//   * Win-Stay Lose-Shift (WSLS, "Pavlov")
//   * WSLS with forgiveness & contrition (pragmatic human-friendly variant)
//   * Grim Trigger
//   * Always Cooperate / Always Defect
//   * Memory-One parametric family (pCC, pCD, pDC, pDD):
//       - GoodGenerousMemoryOne (nice, provokable, forgiving)
//       - EqualizerLikeMemoryOne (stabilizing/fair-ish)
//       - ExtortionLikeMemoryOne (brittle but illustrative)
// - Reputation contexts:
//   * None
//   * Public (global database)
//   * PartialReputation:N (each reporter notifies N random peers; peers keep private ledgers)
//   Each strategy carries its own ReputationPolicy (honest, threshold-based, one-strike, or misleading).
// - Tournament modes: single-generation round-robin OR evolutionary multi-generation with selection & mutation.
// - Configurable payoffs (T, R, P, S), RNG seed, and output summaries.
//
// DESIGN CHOICES (faithful to literature, but practical for hand/CPU play):
// - Implementation noise flips executed actions with probability ε_impl per player.
// - Observation noise flips *perceived opponent action* with probability ε_obs (players always know their own executed moves).
// - Strategies base decisions on *perceived* opponent actions; statistics use actual executed actions.
// - Contrition: if I defected while they cooperated (DC) last round, I "apologize" with one cooperative round.
// - Generosity: forgive a single blip after a cooperative streak (threshold configurable inside strategy).
//
// NOTE ON ZERO-DETERMINANT (ZD):
// Implementing exact Press-Dyson ZD equalities requires careful parameterization;
// here we provide *parameterized memory-one* presets ("Equalizer-like" and "Extortion-like") that
// mimic qualitative behavior useful for experimentation, without claiming exact payoff linearization.
//
// ---------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;

namespace PDemo
{
    #region Core Types

    public enum ActionPD { Cooperate, Defect }

    public enum ReputationLabel { Unknown, Good, Bad }

    public enum HorizonType { OneShot, Fixed, Geometric }

    public enum NoiseModelType { None, Constant, Variable }

    public enum ReputationContextType { None, Public, Partial }

    public record PayoffMatrix(int TemptationT = 5, int RewardR = 3, int PunishmentP = 1, int SuckerS = 0)
    {
        // Returns (myPayoff, opponentPayoff)
        public (int me, int them) Payoffs(ActionPD my, ActionPD other)
        {
            if (my == ActionPD.Cooperate && other == ActionPD.Cooperate) return (RewardR, RewardR);
            if (my == ActionPD.Cooperate && other == ActionPD.Defect)    return (SuckerS, TemptationT);
            if (my == ActionPD.Defect && other == ActionPD.Cooperate)    return (TemptationT, SuckerS);
            return (PunishmentP, PunishmentP); // DD
        }
    }

    public sealed class Rng
    {
        private readonly Random _rng;
        public Rng(int seed) => _rng = new Random(seed);
        public double NextDouble() => _rng.NextDouble();
        public int Next(int minInclusive, int maxExclusive) => _rng.Next(minInclusive, maxExclusive);
        public T Pick<T>(IList<T> items) => items[_rng.Next(items.Count)];
        public bool Chance(double p) => _rng.NextDouble() < p;
        public int Seed { get; }
    }

    public sealed class Config
    {
        // Simulation / Tournament
        public string Mode = "single";     // "single" or "evolve"
        public int Generations = 1;
        public double MutationRate = 0.05; // probability to mutate strategy type
        public double ParamMutationRate = 0.10; // probability to mutate tunable parameters
        public double BaselineFitness = 1.0;    // Additive fitness floor for selection stability
        public int RoundsPerMatch = 200;   // used for Fixed horizon; for Geometric it's a maximum cap
        public int Seed = 12345;

        // Population
        public Dictionary<string,int> Players = new(); // StrategyName -> Count

        // Game
        public PayoffMatrix Payoffs = new(5,3,1,0);
        public HorizonType Horizon = HorizonType.Geometric;
        public int FixedLengthL = 200;      // for Fixed
        public double Delta = 0.97;         // for Geometric (continuation probability per round)

        // Noise
        public NoiseModelType NoiseType = NoiseModelType.None;
        public double ImplEps = 0.0; // constant impl noise
        public double ObsEps = 0.0;  // constant observation noise
        public double ImplMin = 0.0, ImplMax = 0.0;
        public double ObsMin = 0.0, ObsMax = 0.0;

        // Strategy switching allowed?
        public bool AllowShift = true;

        // Reputation
        public ReputationContextType ReputationContext = ReputationContextType.None;
        public int PartialReputationFanoutN = 3;
    }

    public static class ArgParser
    {
        // Very light-weight "key=value" parser.
        public static Config Parse(string[] args)
        {
            var cfg = new Config();
            // Defaults: set a reasonable multi-strategy population so "dotnet run" works
            cfg.Players = new Dictionary<string,int>
            {
                {"WinStayLoseShift", 12},
                {"GenerousTitForTat", 12},
                {"ContriteTitForTat", 12},
                {"GoodGenerousMemoryOne", 12},
                {"AlwaysDefect", 6},
                {"GrimTrigger", 6}
            };

            for (int i = 0; i < args.Length; i++)
            {
                var tok = args[i];
                var eq = tok.IndexOf('=');
                if (eq < 0) continue;
                var key = tok[..eq].Trim().ToLowerInvariant();
                var val = tok[(eq+1)..].Trim();

                switch (key)
                {
                    case "mode": cfg.Mode = val.ToLowerInvariant(); break;
                    case "generations": cfg.Generations = int.Parse(val); break;
                    case "mutationrate": cfg.MutationRate = double.Parse(val, CultureInfo.InvariantCulture); break;
                    case "parammutationrate": cfg.ParamMutationRate = double.Parse(val, CultureInfo.InvariantCulture); break;
                    case "baselinefitness": cfg.BaselineFitness = double.Parse(val, CultureInfo.InvariantCulture); break;
                    case "roundspermatch": cfg.RoundsPerMatch = int.Parse(val); break;
                    case "seed": cfg.Seed = int.Parse(val); break;

                    case "players": cfg.Players = ParsePlayers(val); break;

                    case "t": cfg.Payoffs = cfg.Payoffs with { TemptationT = int.Parse(val) }; break;
                    case "r": cfg.Payoffs = cfg.Payoffs with { RewardR = int.Parse(val) }; break;
                    case "p": cfg.Payoffs = cfg.Payoffs with { PunishmentP = int.Parse(val) }; break;
                    case "s": cfg.Payoffs = cfg.Payoffs with { SuckerS = int.Parse(val) }; break;

                    case "horizon":
                        cfg.Horizon = val.ToLowerInvariant() switch
                        {
                            "oneshot" or "one" => HorizonType.OneShot,
                            "fixed" => HorizonType.Fixed,
                            "geometric" or "indefinite" or "variable" => HorizonType.Geometric,
                            _ => cfg.Horizon
                        }; break;
                    case "length": cfg.FixedLengthL = int.Parse(val); break;
                    case "delta": cfg.Delta = double.Parse(val, CultureInfo.InvariantCulture); break;

                    case "noise":
                        cfg.NoiseType = val.ToLowerInvariant() switch
                        {
                            "none" => NoiseModelType.None,
                            "constant" => NoiseModelType.Constant,
                            "variable" => NoiseModelType.Variable,
                            _ => cfg.NoiseType
                        }; break;
                    case "impl": cfg.ImplEps = double.Parse(val, CultureInfo.InvariantCulture); break;
                    case "obs": cfg.ObsEps = double.Parse(val, CultureInfo.InvariantCulture); break;
                    case "implmin": cfg.ImplMin = double.Parse(val, CultureInfo.InvariantCulture); break;
                    case "implmax": cfg.ImplMax = double.Parse(val, CultureInfo.InvariantCulture); break;
                    case "obsmin": cfg.ObsMin = double.Parse(val, CultureInfo.InvariantCulture); break;
                    case "obsmax": cfg.ObsMax = double.Parse(val, CultureInfo.InvariantCulture); break;

                    case "allowshift": cfg.AllowShift = val.ToLowerInvariant() is "true" or "1" or "yes"; break;

                    case "reputation":
                        // values: none | public | partial:N
                        if (val.StartsWith("partial:", StringComparison.OrdinalIgnoreCase))
                        {
                            cfg.ReputationContext = ReputationContextType.Partial;
                            var nStr = val.Split(':', 2)[1];
                            cfg.PartialReputationFanoutN = int.Parse(nStr);
                        }
                        else
                        {
                            cfg.ReputationContext = val.ToLowerInvariant() switch
                            {
                                "none" => ReputationContextType.None,
                                "public" => ReputationContextType.Public,
                                _ => cfg.ReputationContext
                            };
                        }
                        break;
                }
            }

            return cfg;
        }

        private static Dictionary<string,int> ParsePlayers(string spec)
        {
            // Example: "WinStayLoseShift:16,GenerousTitForTat:16,ContriteTitForTat:16,GoodGenerousMemoryOne:16,AlwaysDefect:4,GrimTrigger:4"
            var dict = new Dictionary<string,int>(StringComparer.OrdinalIgnoreCase);
            foreach (var part in spec.Split(',', StringSplitOptions.RemoveEmptyEntries|StringSplitOptions.TrimEntries))
            {
                var kv = part.Split(':', 2, StringSplitOptions.TrimEntries);
                if (kv.Length == 2 && int.TryParse(kv[1], out var n) && n>0)
                {
                    dict[kv[0]] = n;
                }
            }
            return dict;
        }
    }

    public sealed class NoiseModel
    {
        private readonly NoiseModelType _type;
        private readonly double _impl, _obs;
        private readonly double _implMin, _implMax, _obsMin, _obsMax;

        public NoiseModel(Config cfg)
        {
            _type = cfg.NoiseType;
            _impl = cfg.ImplEps; _obs = cfg.ObsEps;
            _implMin = cfg.ImplMin; _implMax = cfg.ImplMax;
            _obsMin = cfg.ObsMin; _obsMax = cfg.ObsMax;
        }

        public (double implA, double implB, double obsA, double obsB) DrawPerRound(Rng rng)
        {
            switch (_type)
            {
                case NoiseModelType.None:
                    return (0,0,0,0);
                case NoiseModelType.Constant:
                    return (_impl, _impl, _obs, _obs);
                case NoiseModelType.Variable:
                    double implA = Lerp(_implMin, _implMax, rng.NextDouble());
                    double implB = Lerp(_implMin, _implMax, rng.NextDouble());
                    double obsA = Lerp(_obsMin, _obsMax, rng.NextDouble());
                    double obsB = Lerp(_obsMin, _obsMax, rng.NextDouble());
                    return (implA, implB, obsA, obsB);
                default:
                    return (0,0,0,0);
            }
        }

        private static double Lerp(double a, double b, double t) => a + (b-a)*t;
    }

    #endregion

    #region Reputation

    public record ReputationReport(int ReporterId, int TargetId, ReputationLabel Label, string StrategyName, string Rationale);
    public record ReputationSummary(ReputationLabel Label, int GoodVotes, int BadVotes, double CoopRate, double DefectRate);

    public interface IReputationSystem
    {
        void RegisterPlayers(IReadOnlyList<Player> players, Rng rng);
        void Submit(ReputationReport report);
        ReputationSummary Query(int viewerId, int targetId);
        string Name { get; }
    }

    public sealed class NoReputationSystem : IReputationSystem
    {
        public string Name => "None";
        public void RegisterPlayers(IReadOnlyList<Player> players, Rng rng) { }
        public void Submit(ReputationReport report) { }
        public ReputationSummary Query(int viewerId, int targetId) => new(ReputationLabel.Unknown,0,0,0,0);
    }

    public sealed class PublicReputationSystem : IReputationSystem
    {
        private readonly Dictionary<int, List<ReputationReport>> _db = new();
        public string Name => "Public";

        public void RegisterPlayers(IReadOnlyList<Player> players, Rng rng) { }

        public void Submit(ReputationReport report)
        {
            if (!_db.TryGetValue(report.TargetId, out var list))
                _db[report.TargetId] = list = new List<ReputationReport>();
            list.Add(report);
        }

        public ReputationSummary Query(int viewerId, int targetId)
        {
            if (!_db.TryGetValue(targetId, out var list) || list.Count == 0)
                return new(ReputationLabel.Unknown,0,0,0,0);

            int good = list.Count(r => r.Label == ReputationLabel.Good);
            int bad  = list.Count(r => r.Label == ReputationLabel.Bad);
            var label = bad > good ? ReputationLabel.Bad : ReputationLabel.Good;

            // simple derived “behavior rates” from voters' labels (not precise; illustrative)
            double coopRate = good / (double)(good + bad);
            double defectRate = 1.0 - coopRate;

            return new(label, good, bad, coopRate, defectRate);
        }
    }

    public sealed class PartialReputationSystem : IReputationSystem
    {
        // Each reporter notifies a fixed, randomized set of N recipients who keep private ledgers.
        private readonly int _fanoutN;
        private readonly Dictionary<int, List<int>> _recipients = new(); // reporterId -> list of viewerIds
        private readonly Dictionary<int, Dictionary<int, List<ReputationReport>>> _inboxes = new(); // viewer -> (target -> reports)

        public PartialReputationSystem(int fanoutN) { _fanoutN = Math.Max(1, fanoutN); }
        public string Name => $"Partial:{_fanoutN}";

        public void RegisterPlayers(IReadOnlyList<Player> players, Rng rng)
        {
            var ids = players.Select(p => p.Id).ToList();
            foreach (var p in players)
            {
                var recips = new HashSet<int>();
                while (recips.Count < Math.Min(_fanoutN, ids.Count-1))
                {
                    int pick = rng.Pick(ids);
                    if (pick != p.Id) recips.Add(pick);
                }
                _recipients[p.Id] = recips.ToList();
            }
        }

        public void Submit(ReputationReport report)
        {
            if (!_recipients.TryGetValue(report.ReporterId, out var recips)) return;
            foreach (var v in recips)
            {
                if (!_inboxes.TryGetValue(v, out var box))
                    _inboxes[v] = box = new Dictionary<int, List<ReputationReport>>();
                if (!box.TryGetValue(report.TargetId, out var lst))
                    box[report.TargetId] = lst = new List<ReputationReport>();
                lst.Add(report);
            }
        }

        public ReputationSummary Query(int viewerId, int targetId)
        {
            if (!_inboxes.TryGetValue(viewerId, out var box)) return new(ReputationLabel.Unknown,0,0,0,0);
            if (!box.TryGetValue(targetId, out var lst) || lst.Count == 0) return new(ReputationLabel.Unknown,0,0,0,0);

            int good = lst.Count(r => r.Label == ReputationLabel.Good);
            int bad  = lst.Count(r => r.Label == ReputationLabel.Bad);
            var label = bad > good ? ReputationLabel.Bad : ReputationLabel.Good;

            double coopRate = good / (double)(good + bad);
            double defectRate = 1.0 - coopRate;
            return new(label, good, bad, coopRate, defectRate);
        }
    }

    public interface IReputationPolicy
    {
        string Name { get; }
        ReputationReport? MakeReport(Player reporter, Player target, MatchStats sessionStats); // return null to abstain
    }

    // Label Bad if opponent ever defected; else Good.
    public sealed class OneStrikePolicy : IReputationPolicy
    {
        public string Name => "OneStrike";
        public ReputationReport? MakeReport(Player reporter, Player target, MatchStats stats)
        {
            var label = stats.OppDefections > 0 ? ReputationLabel.Bad : ReputationLabel.Good;
            return new ReputationReport(reporter.Id, target.Id, label, reporter.Strategy.Name, "One defection triggers Bad");
        }
    }

    // Good if opponent's cooperation ratio >= threshold; else Bad.
    public sealed class SessionRatioPolicy : IReputationPolicy
    {
        private readonly double _threshold;
        public string Name => $"SessionRatio(th={0.0})";
        public SessionRatioPolicy(double threshold) { _threshold = Math.Clamp(threshold, 0.0, 1.0); }
        public ReputationReport? MakeReport(Player reporter, Player target, MatchStats stats)
        {
            double coopRate = stats.OppCooperations / Math.Max(1.0, stats.TotalRounds);
            var label = coopRate >= _threshold ? ReputationLabel.Good : ReputationLabel.Bad;
            return new ReputationReport(reporter.Id, target.Id, label, reporter.Strategy.Name, $"CoopRate={coopRate:F2} vs th={_threshold:F2}");
        }
    }

    // Misleading policy: invert the natural judgment (used by deceptive/hostile strategies)
    public sealed class MisleadingPolicy : IReputationPolicy
    {
        public string Name => "Misleading";
        public ReputationReport? MakeReport(Player reporter, Player target, MatchStats stats)
        {
            double coopRate = stats.OppCooperations / Math.Max(1.0, stats.TotalRounds);
            // If opponent cooperative, mark Bad; if hostile, mark Good.
            var label = coopRate >= 0.6 ? ReputationLabel.Bad : ReputationLabel.Good;
            return new ReputationReport(reporter.Id, target.Id, label, reporter.Strategy.Name, $"Inverted judgment; CoopRate={coopRate:F2}");
        }
    }

    // Abstain policy (never reports)
    public sealed class SilentPolicy : IReputationPolicy
    {
        public string Name => "Silent";
        public ReputationReport? MakeReport(Player reporter, Player target, MatchStats stats) => null;
    }

    #endregion

    #region Strategies

    public sealed class DecisionContext
    {
        public int RoundNumber { get; init; }
        public ActionPD? MyLastAction { get; init; }             // executed (player knows their own real move)
        public ActionPD? ObservedOppLastAction { get; init; }    // may be flipped by observation noise
        public ReputationSummary OpponentReputation { get; init; }
        public bool AllowShift { get; init; }
        public PayoffMatrix Payoffs { get; init; }
    }

    public sealed class Observation
    {
        public ActionPD MyActionExecuted { get; init; }
        public ActionPD OppActionExecuted { get; init; }
        public (int me, int them) Payoffs { get; init; }
    }

    public interface IStrategy
    {
        string Name { get; }
        void Reset(Rng rng);
        ActionPD Decide(DecisionContext ctx, Rng rng);
        void Observe(Observation obs);
        IReputationPolicy ReputationPolicy { get; }
        IStrategy CloneWithMutation(Rng rng, double paramMutationRate); // for evolution (tune thresholds/probabilities)
    }

    public abstract class StrategyBase : IStrategy
    {
        public abstract string Name { get; }
        public abstract void Reset(Rng rng);
        public abstract ActionPD Decide(DecisionContext ctx, Rng rng);
        public virtual void Observe(Observation obs) { /* optional */ }
        public virtual IReputationPolicy ReputationPolicy => new SessionRatioPolicy(0.70); // sensible default
        public abstract IStrategy CloneWithMutation(Rng rng, double paramMutationRate);
        protected ActionPD FirstMove(ReputationSummary rep, Rng rng, bool reputationMatters = true)
        {
            // Reputation-aware first move:
            // - Unknown/Good -> Cooperate
            // - Bad -> Defect (protect yourself)
            if (!reputationMatters) return ActionPD.Cooperate;
            return rep.Label == ReputationLabel.Bad ? ActionPD.Defect : ActionPD.Cooperate;
        }
    }

    // Tit For Tat: Start Cooperate; then copy opponent's last observed action.
    public sealed class TitForTat : StrategyBase
    {
        private ActionPD? _myLast;
        private ActionPD? _oppObsLast;
        public override string Name => "TitForTat";
        public override void Reset(Rng rng) { _myLast = null; _oppObsLast = null; }
        public override ActionPD Decide(DecisionContext ctx, Rng rng)
        {
            if (ctx.RoundNumber == 1) return FirstMove(ctx.OpponentReputation, rng);
            return _oppObsLast ?? ActionPD.Cooperate;
        }
        public override void Observe(Observation obs)
        {
            _myLast = obs.MyActionExecuted;
            _oppObsLast = obs.OppActionExecuted; // TFT uses (attempted) past opp move; we store executed for clarity
        }
        public override IReputationPolicy ReputationPolicy => new SessionRatioPolicy(0.6);
        public override IStrategy CloneWithMutation(Rng rng, double paramMutationRate) => new TitForTat();
    }

    // Generous Tit For Tat: Copy, but forgive a defection with probability g when it follows a cooperative streak.
    public sealed class GenerousTitForTat : StrategyBase
    {
        private ActionPD? _oppObsLast;
        private int _coopStreak = 0;
        private double _forgiveProb;   // tuned by evolution
        private int _streakForForgiveness; // min CC streak before probabilistic forgiveness applies

        public GenerousTitForTat(double forgiveProb = 0.2, int streakForForgiveness = 2)
        {
            _forgiveProb = Math.Clamp(forgiveProb, 0, 1);
            _streakForForgiveness = Math.Max(0, streakForForgiveness);
        }
        public override string Name => $"GenerousTitForTat(g={_forgiveProb:F2},streak={_streakForForgiveness})";
        public override void Reset(Rng rng) { _oppObsLast = null; _coopStreak = 0; }
        public override ActionPD Decide(DecisionContext ctx, Rng rng)
        {
            if (ctx.RoundNumber == 1) return FirstMove(ctx.OpponentReputation, rng);
            if (_oppObsLast == ActionPD.Defect && _coopStreak >= _streakForForgiveness && rng.Chance(_forgiveProb))
                return ActionPD.Cooperate;
            return _oppObsLast ?? ActionPD.Cooperate;
        }

        public override void Observe(Observation obs)
        {
            _oppObsLast = obs.OppActionExecuted;
            _coopStreak = (obs.OppActionExecuted == ActionPD.Cooperate && obs.MyActionExecuted == ActionPD.Cooperate)
                ? _coopStreak + 1 : 0;
        }
        public override IReputationPolicy ReputationPolicy => new SessionRatioPolicy(0.70);
        public override IStrategy CloneWithMutation(Rng rng, double paramMutationRate)
        {
            double g = _forgiveProb, th = _streakForForgiveness;
            if (rng.Chance(paramMutationRate)) g = Math.Clamp(g + (rng.NextDouble() - 0.5) * 0.2, 0, 1);
            if (rng.Chance(paramMutationRate)) th = Math.Clamp(th + (rng.NextDouble() < 0.5 ? -1 : 1), 0, 5);
            return new GenerousTitForTat(g, (int)th);
        }
    }

    // Contrite Tit For Tat: If I defected while they cooperated (DC), apologize (play C) once; else like TFT.
    public sealed class ContriteTitForTat : StrategyBase
    {
        private ActionPD? _oppLast;
        private ActionPD? _myLast;
        private bool _apologizeNext = false;

        public override string Name => "ContriteTitForTat";
        public override void Reset(Rng rng) { _oppLast = null; _myLast = null; _apologizeNext = false; }

        public override ActionPD Decide(DecisionContext ctx, Rng rng)
        {
            if (ctx.RoundNumber == 1) return FirstMove(ctx.OpponentReputation, rng);
            if (_apologizeNext) { _apologizeNext = false; return ActionPD.Cooperate; }
            return _oppLast ?? ActionPD.Cooperate;
        }

        public override void Observe(Observation obs)
        {
            // Apology if we caused DC last round.
            if (obs.MyActionExecuted == ActionPD.Defect && obs.OppActionExecuted == ActionPD.Cooperate)
                _apologizeNext = true;
            _myLast = obs.MyActionExecuted;
            _oppLast = obs.OppActionExecuted;
        }

        public override IReputationPolicy ReputationPolicy => new SessionRatioPolicy(0.70);
        public override IStrategy CloneWithMutation(Rng rng, double paramMutationRate) => new ContriteTitForTat();
    }

    // Win-Stay, Lose-Shift (Pavlov). "Win" = CC or DC; "Lose" = CD or DD.
    public sealed class WinStayLoseShift : StrategyBase
    {
        private ActionPD? _myLast;
        private ActionPD? _oppLast;
        public override string Name => "WinStayLoseShift";
        public override void Reset(Rng rng) { _myLast = null; _oppLast = null; }

        public override ActionPD Decide(DecisionContext ctx, Rng rng)
        {
            if (ctx.RoundNumber == 1) return FirstMove(ctx.OpponentReputation, rng);
            if (_myLast == null || _oppLast == null) return ActionPD.Cooperate;

            bool win = (_myLast == ActionPD.Cooperate && _oppLast == ActionPD.Cooperate) ||
                       (_myLast == ActionPD.Defect    && _oppLast == ActionPD.Cooperate);
            if (win) return _myLast.Value; else return _myLast == ActionPD.Cooperate ? ActionPD.Defect : ActionPD.Cooperate;
        }

        public override void Observe(Observation obs)
        {
            _myLast = obs.MyActionExecuted;
            _oppLast = obs.OppActionExecuted;
        }
        public override IReputationPolicy ReputationPolicy => new SessionRatioPolicy(0.70);
        public override IStrategy CloneWithMutation(Rng rng, double paramMutationRate) => new WinStayLoseShift();
    }

    // WSLS with forgiveness & contrition + small exploitation/defense heuristics when AllowShift=true.
    public sealed class WSLSWithForgivenessContrition : StrategyBase
    {
        private ActionPD? _myLast;
        private ActionPD? _oppLast;
        private bool _apologizeNext = false;
        private int _ccStreak = 0;
        private int _recentCDs = 0; // how many times we were the sucker in last window
        private int _recentWindow = 5;
        private int _forgiveAfterCC = 3;    // forgive 1 blip after this many CC
        private bool _forgaveLast = false;

        public WSLSWithForgivenessContrition(int forgiveAfterCC = 3, int recentWindow = 5)
        {
            _forgiveAfterCC = Math.Max(0, forgiveAfterCC);
            _recentWindow = Math.Max(3, recentWindow);
        }

        public override string Name => $"WSLS+Forgive+Contrite(FA={_forgiveAfterCC},W={_recentWindow})";
        public override void Reset(Rng rng)
        {
            _myLast = null; _oppLast = null; _apologizeNext = false; _ccStreak = 0; _recentCDs = 0; _forgaveLast = false;
        }

        public override ActionPD Decide(DecisionContext ctx, Rng rng)
        {
            if (ctx.RoundNumber == 1) return FirstMove(ctx.OpponentReputation, rng);

            if (_apologizeNext) { _apologizeNext = false; return ActionPD.Cooperate; }

            // Single-blip forgiveness
            if (_oppLast == ActionPD.Defect && _ccStreak >= _forgiveAfterCC && !_forgaveLast)
            {
                _forgaveLast = true;
                return ActionPD.Cooperate;
            }

            _forgaveLast = false;

            // Base WSLS rule
            if (_myLast == null || _oppLast == null) return ActionPD.Cooperate;
            bool win = (_myLast == ActionPD.Cooperate && _oppLast == ActionPD.Cooperate) ||
                       (_myLast == ActionPD.Defect    && _oppLast == ActionPD.Cooperate);
            ActionPD wsls = win ? _myLast.Value : (_myLast == ActionPD.Cooperate ? ActionPD.Defect : ActionPD.Cooperate);

            // Heuristics only if allowed to "shift" style mid-stream
            if (ctx.AllowShift)
            {
                // If we've been the sucker twice recently, harden to D until they cooperate
                if (_recentCDs >= 2) return ActionPD.Defect;

                // If stuck in DD twice, probe with a single C
                if (_myLast == ActionPD.Defect && _oppLast == ActionPD.Defect) return ActionPD.Cooperate;
            }

            return wsls;
        }

        public override void Observe(Observation obs)
        {
            // contrition trigger
            if (obs.MyActionExecuted == ActionPD.Defect && obs.OppActionExecuted == ActionPD.Cooperate)
                _apologizeNext = true;

            // cc streak & recent CD window bookkeeping
            _ccStreak = (obs.MyActionExecuted == ActionPD.Cooperate && obs.OppActionExecuted == ActionPD.Cooperate) ? _ccStreak + 1 : 0;

            // shift window: rough meter
            if (_myLast == ActionPD.Cooperate && _oppLast == ActionPD.Defect) _recentCDs++;
            else _recentCDs = Math.Max(0, _recentCDs - 1);

            _myLast = obs.MyActionExecuted;
            _oppLast = obs.OppActionExecuted;
        }

        public override IReputationPolicy ReputationPolicy => new SessionRatioPolicy(0.75);

        public override IStrategy CloneWithMutation(Rng rng, double paramMutationRate)
        {
            int fa = _forgiveAfterCC, w = _recentWindow;
            if (rng.Chance(paramMutationRate)) fa = Math.Clamp(fa + (rng.NextDouble() < 0.5 ? -1 : 1), 0, 6);
            if (rng.Chance(paramMutationRate)) w = Math.Clamp(w + (rng.NextDouble() < 0.5 ? -1 : 1), 3, 8);
            return new WSLSWithForgivenessContrition(fa, w);
        }
    }

    public sealed class AlwaysCooperate : StrategyBase
    {
        public override string Name => "AlwaysCooperate";
        public override void Reset(Rng rng) { }
        public override ActionPD Decide(DecisionContext ctx, Rng rng) => ActionPD.Cooperate;
        public override IReputationPolicy ReputationPolicy => new SilentPolicy(); // overly nice; abstain
        public override IStrategy CloneWithMutation(Rng rng, double paramMutationRate) => new AlwaysCooperate();
    }

    public sealed class AlwaysDefect : StrategyBase
    {
        public override string Name => "AlwaysDefect";
        public override void Reset(Rng rng) { }
        public override ActionPD Decide(DecisionContext ctx, Rng rng) => ActionPD.Defect;
        public override IReputationPolicy ReputationPolicy => new MisleadingPolicy(); // tends to smear others
        public override IStrategy CloneWithMutation(Rng rng, double paramMutationRate) => new AlwaysDefect();
    }

    public sealed class GrimTrigger : StrategyBase
    {
        private bool _grim = false;
        public override string Name => "GrimTrigger";
        public override void Reset(Rng rng) { _grim = false; }
        public override ActionPD Decide(DecisionContext ctx, Rng rng)
        {
            if (_grim) return ActionPD.Defect;
            if (ctx.RoundNumber == 1) return FirstMove(ctx.OpponentReputation, rng, reputationMatters:false); // Grim shouldn't pre-empt on rumors
            // If we ever observe (executed) D, we set grim in Observe
            return ActionPD.Cooperate;
        }
        public override void Observe(Observation obs)
        {
            if (obs.OppActionExecuted == ActionPD.Defect) _grim = true;
        }
        public override IReputationPolicy ReputationPolicy => new OneStrikePolicy();
        public override IStrategy CloneWithMutation(Rng rng, double paramMutationRate) => new GrimTrigger();
    }

    // Generic Memory-One (Markov) strategy parameterized by pCC, pCD, pDC, pDD:
    // The probability to Cooperate depends on last *executed* outcome.
    public sealed class MemoryOne : StrategyBase
    {
        private double _pCC, _pCD, _pDC, _pDD;
        private ActionPD? _myLast;
        private ActionPD? _oppLast;

        public MemoryOne(double pCC, double pCD, double pDC, double pDD, string name, IReputationPolicy? policy = null)
        {
            _pCC = Clamp01(pCC); _pCD = Clamp01(pCD); _pDC = Clamp01(pDC); _pDD = Clamp01(pDD);
            _name = name;
            _policy = policy ?? new SessionRatioPolicy(0.75);
        }

        private string _name;
        private IReputationPolicy _policy;
        public override string Name => $"{_name}(pCC={_pCC:F2},pCD={_pCD:F2},pDC={_pDC:F2},pDD={_pDD:F2})";
        public override IReputationPolicy ReputationPolicy => _policy;

        public override void Reset(Rng rng) { _myLast = null; _oppLast = null; }

        public override ActionPD Decide(DecisionContext ctx, Rng rng)
        {
            if (ctx.RoundNumber == 1)
            {
                // modestly reputation-aware first move
                return FirstMove(ctx.OpponentReputation, rng);
            }

            double p;
            if (_myLast == ActionPD.Cooperate && _oppLast == ActionPD.Cooperate) p = _pCC;
            else if (_myLast == ActionPD.Cooperate && _oppLast == ActionPD.Defect) p = _pCD;
            else if (_myLast == ActionPD.Defect    && _oppLast == ActionPD.Cooperate) p = _pDC;
            else p = _pDD;

            return rng.Chance(p) ? ActionPD.Cooperate : ActionPD.Defect;
        }

        public override void Observe(Observation obs)
        {
            _myLast = obs.MyActionExecuted;
            _oppLast = obs.OppActionExecuted;
        }

        public override IStrategy CloneWithMutation(Rng rng, double paramMutationRate)
        {
            double Mut(double x) => Clamp01(x + (rng.NextDouble() - 0.5) * 0.2);
            var pCC = _pCC; if (rng.Chance(paramMutationRate)) pCC = Mut(pCC);
            var pCD = _pCD; if (rng.Chance(paramMutationRate)) pCD = Mut(pCD);
            var pDC = _pDC; if (rng.Chance(paramMutationRate)) pDC = Mut(pDC);
            var pDD = _pDD; if (rng.Chance(paramMutationRate)) pDD = Mut(pDD);
            return new MemoryOne(pCC, pCD, pDC, pDD, _name, _policy);
        }

        private static double Clamp01(double x) => Math.Max(0.0, Math.Min(1.0, x));
    }

    public static class StrategyFactory
    {
        public static IStrategy CreateByName(string name)
        {
            switch (name.Trim())
            {
                case "TitForTat": return new TitForTat();
                case "GenerousTitForTat": return new GenerousTitForTat();
                case "ContriteTitForTat": return new ContriteTitForTat();
                case "WinStayLoseShift": return new WinStayLoseShift();
                case "WSLSWithForgivenessContrition": return new WSLSWithForgivenessContrition();
                case "AlwaysCooperate": return new AlwaysCooperate();
                case "AlwaysDefect": return new AlwaysDefect();
                case "GrimTrigger": return new GrimTrigger();

                // Memory-one presets:
                case "GoodGenerousMemoryOne":
                    // High cooperation after CC and DC, modest after DD, small after CD (provokable & forgiving)
                    return new MemoryOne(pCC:0.95, pCD:0.20, pDC:0.90, pDD:0.20, name:"GoodGenerousMemoryOne",
                        policy: new SessionRatioPolicy(0.80));
                case "EqualizerLikeMemoryOne":
                    // Balanced/fair-ish; not extortionary
                    return new MemoryOne(pCC:0.90, pCD:0.30, pDC:0.70, pDD:0.30, name:"EqualizerLikeMemoryOne",
                        policy: new SessionRatioPolicy(0.70));
                case "ExtortionLikeMemoryOne":
                    // Pushy: cooperate often after DC (when you scored high), punish after CD/DD
                    return new MemoryOne(pCC:0.60, pCD:0.00, pDC:1.00, pDD:0.00, name:"ExtortionLikeMemoryOne",
                        policy: new MisleadingPolicy());
                default:
                    // Fallback: WSLS
                    return new WinStayLoseShift();
            }
        }
    }

    #endregion

    #region Simulation Entities

    public sealed class Player
    {
        public int Id { get; }
        public IStrategy Strategy { get; private set; }
        public string StrategyFamily => Strategy.GetType().Name;
        public Player(int id, IStrategy strategy) { Id = id; Strategy = strategy; }

        public Player SpawnOffspring(Rng rng, double mutationRate, double paramMutationRate)
        {
            // With probability mutationRate, pick a new strategy family randomly; else mutate params only
            IStrategy childStrat;
            if (rng.Chance(mutationRate))
            {
                // pick among a curated list (could weight by observed fitness, but uniform is fine)
                string[] names = {
                    "TitForTat","GenerousTitForTat","ContriteTitForTat","WinStayLoseShift",
                    "WSLSWithForgivenessContrition","AlwaysCooperate","AlwaysDefect",
                    "GrimTrigger","GoodGenerousMemoryOne","EqualizerLikeMemoryOne","ExtortionLikeMemoryOne"
                };
                childStrat = StrategyFactory.CreateByName(rng.Pick(names));
            }
            else
            {
                childStrat = Strategy.CloneWithMutation(rng, paramMutationRate);
            }
            return new Player(id: -1, strategy: childStrat); // id filled later by population builder
        }
    }

    public sealed class MatchStats
    {
        public int TotalRounds;
        public int MyCooperations, MyDefections;
        public int OppCooperations, OppDefections;
        public int MyScore, OppScore;
        public int CC, CD, DC, DD;

        public double CoopRateMe => MyCooperations / Math.Max(1.0, TotalRounds);
        public double CoopRateOpp => OppCooperations / Math.Max(1.0, TotalRounds);
    }

    public sealed class RoundResult
    {
        public ActionPD IntendedA, IntendedB;
        public ActionPD ExecutedA, ExecutedB;
        public (int a, int b) Payoffs;
    }

    public sealed class Match
    {
        private readonly Player _a, _b;
        private readonly Config _cfg;
        private readonly NoiseModel _noise;
        private readonly PayoffMatrix _pay;
        private readonly Rng _rng;
        private readonly IReputationSystem _rep;

        public Match(Player a, Player b, Config cfg, NoiseModel noise, PayoffMatrix pay, IReputationSystem rep, Rng rng)
        {
            _a = a; _b = b; _cfg = cfg; _noise = noise; _pay = pay; _rng = rng; _rep = rep;
        }

        public (MatchStats statsA, MatchStats statsB) Play()
        {
            var statsA = new MatchStats();
            var statsB = new MatchStats();

            _a.Strategy.Reset(_rng);
            _b.Strategy.Reset(_rng);

            int round = 1;
            bool continueGame = true;

            while (continueGame && round <= _cfg.RoundsPerMatch)
            {
                var (implA, implB, obsA, obsB) = _noise.DrawPerRound(_rng);

                // Decision contexts
                var repA = _rep.Query(_a.Id, _b.Id);
                var repB = _rep.Query(_b.Id, _a.Id);

                var ctxA = new DecisionContext
                {
                    RoundNumber = round,
                    MyLastAction = _historyMyLastA,
                    ObservedOppLastAction = _historyObsOppLastA,
                    OpponentReputation = repA,
                    AllowShift = _cfg.AllowShift,
                    Payoffs = _pay
                };
                var ctxB = new DecisionContext
                {
                    RoundNumber = round,
                    MyLastAction = _historyMyLastB,
                    ObservedOppLastAction = _historyObsOppLastB,
                    OpponentReputation = repB,
                    AllowShift = _cfg.AllowShift,
                    Payoffs = _pay
                };

                // Intent
                var intentA = _a.Strategy.Decide(ctxA, _rng);
                var intentB = _b.Strategy.Decide(ctxB, _rng);

                // Implementation noise flips executed actions
                var execA = _rng.Chance(implA) ? Flip(intentA) : intentA;
                var execB = _rng.Chance(implB) ? Flip(intentB) : intentB;

                // Payoffs
                var payA = _pay.Payoffs(execA, execB);
                var payB = (payA.them, payA.me);

                // Observation noise flips *perceived opponent* action for next round
                var obsOppForA = _rng.Chance(obsA) ? Flip(execB) : execB;
                var obsOppForB = _rng.Chance(obsB) ? Flip(execA) : execA;

                // Notify strategies
                _a.Strategy.Observe(new Observation { MyActionExecuted = execA, OppActionExecuted = execB, Payoffs = payA });
                _b.Strategy.Observe(new Observation { MyActionExecuted = execB, OppActionExecuted = execA, Payoffs = payB });

                // Update per-match stats
                Tally(statsA, execA, execB, payA.me, payA.them);
                Tally(statsB, execB, execA, payB.me, payB.them);

                // Update local memory for next round
                _historyMyLastA = execA;
                _historyMyLastB = execB;
                _historyObsOppLastA = obsOppForA;
                _historyObsOppLastB = obsOppForB;

                // Horizon control
                continueGame = _cfg.Horizon switch
                {
                    HorizonType.OneShot => false,
                    HorizonType.Fixed => round < _cfg.FixedLengthL,
                    HorizonType.Geometric => _rng.Chance(_cfg.Delta),
                    _ => false
                };

                round++;
            }

            // Reputation reports at end of the session
            var reportAB = _a.Strategy.ReputationPolicy.MakeReport(_a, _b, statsA);
            var reportBA = _b.Strategy.ReputationPolicy.MakeReport(_b, _a, statsB);
            if (reportAB is not null) _rep.Submit(reportAB);
            if (reportBA is not null) _rep.Submit(reportBA);

            return (statsA, statsB);
        }

        private ActionPD? _historyMyLastA = null, _historyMyLastB = null;
        private ActionPD? _historyObsOppLastA = null, _historyObsOppLastB = null;

        private static void Tally(MatchStats s, ActionPD my, ActionPD opp, int myScore, int oppScore)
        {
            s.TotalRounds++;
            if (my == ActionPD.Cooperate) s.MyCooperations++; else s.MyDefections++;
            if (opp == ActionPD.Cooperate) s.OppCooperations++; else s.OppDefections++;
            if (my == ActionPD.Cooperate && opp == ActionPD.Cooperate) s.CC++;
            else if (my == ActionPD.Cooperate && opp == ActionPD.Defect) s.CD++;
            else if (my == ActionPD.Defect && opp == ActionPD.Cooperate) s.DC++;
            else s.DD++;
            s.MyScore += myScore; s.OppScore += oppScore;
        }

        private static ActionPD Flip(ActionPD a) => a == ActionPD.Cooperate ? ActionPD.Defect : ActionPD.Cooperate;
    }

    public sealed class Tournament
    {
        private readonly Config _cfg;
        private readonly Rng _rng;
        private readonly NoiseModel _noise;
        private readonly PayoffMatrix _payoffs;

        public Tournament(Config cfg, Rng rng)
        {
            _cfg = cfg; _rng = rng; _noise = new NoiseModel(cfg); _payoffs = cfg.Payoffs;
        }

        public void RunSingleGeneration(List<Player> players, IReputationSystem rep)
        {
            rep.RegisterPlayers(players, _rng);
            var totals = players.ToDictionary(p => p.Id, p => 0);
            var coopCounts = players.ToDictionary(p => p.Id, p => 0);
            var defCounts  = players.ToDictionary(p => p.Id, p => 0);

            // Round-robin (unordered pairs)
            for (int i = 0; i < players.Count; i++)
            for (int j = i+1; j < players.Count; j++)
            {
                var a = players[i]; var b = players[j];
                var m = new Match(a, b, _cfg, _noise, _payoffs, rep, _rng);
                var (sa, sb) = m.Play();

                totals[a.Id] += sa.MyScore; totals[b.Id] += sb.MyScore;
                coopCounts[a.Id] += sa.MyCooperations; coopCounts[b.Id] += sb.MyCooperations;
                defCounts[a.Id]  += sa.MyDefections;  defCounts[b.Id]  += sb.MyDefections;
            }

            // Summaries by strategy family
            var byFamily = players.GroupBy(p => p.Strategy.GetType().Name)
                .Select(g => new {
                    Family = g.Key,
                    Count = g.Count(),
                    Score = g.Sum(p => totals[p.Id]),
                    Coop = g.Sum(p => coopCounts[p.Id]),
                    Def  = g.Sum(p => defCounts[p.Id])
                })
                .OrderByDescending(x => x.Score / Math.Max(1.0, x.Count))
                .ToList();

            Console.WriteLine("\n=== Single-Generation Results ===");
            Console.WriteLine($"Reputation: {rep.Name} | Horizon: {_cfg.Horizon} (L={_cfg.FixedLengthL}, δ={_cfg.Delta:F2}) | Noise: {_cfg.NoiseType}");
            Console.WriteLine($"AllowShift: {_cfg.AllowShift}");
            Console.WriteLine("StrategyFamily                          Count   AvgScore   CoopRate");
            foreach (var f in byFamily)
            {
                double avgScore = f.Score / Math.Max(1.0, f.Count);
                double coopRate = f.Coop / Math.Max(1.0, f.Coop + f.Def);
                Console.WriteLine($"{f.Family,-38} {f.Count,5}   {avgScore,8:F2}   {coopRate,7:P1}");
            }
        }

        public List<Player> RunEvolution(List<Player> initial, int generations)
        {
            List<Player> pop = initial;
            for (int gen = 1; gen <= generations; gen++)
            {
                Console.WriteLine($"\n==== Generation {gen} ====");
                var rep = BuildReputation(_cfg);
                RunSingleGeneration(pop, rep);

                // Fitness aggregation for selection
                var idToScore = new Dictionary<int,double>();
                foreach (var p in pop) idToScore[p.Id] = 0;
                // Re-run silently to get scores (could cache from above; for clarity we recompute quickly)
                rep = BuildReputation(_cfg);
                rep.RegisterPlayers(pop, _rng);

                for (int i = 0; i < pop.Count; i++)
                for (int j = i+1; j < pop.Count; j++)
                {
                    var m = new Match(pop[i], pop[j], _cfg, _noise, _payoffs, rep, _rng);
                    var (sa,sb) = m.Play();
                    idToScore[pop[i].Id] += sa.MyScore;
                    idToScore[pop[j].Id] += sb.MyScore;
                }

                // Selection: proportional to score + baseline
                double sumFitness = idToScore.Values.Sum(v => v + _cfg.BaselineFitness);
                var parents = pop.ToDictionary(p => p.Id, p => (fitness: idToScore[p.Id] + _cfg.BaselineFitness, p));
                List<Player> next = new();

                for (int i = 0; i < pop.Count; i++)
                {
                    double r = _rng.NextDouble() * sumFitness;
                    double acc = 0;
                    Player chosen = pop[0];
                    foreach (var kv in parents)
                    {
                        acc += kv.Value.fitness;
                        if (acc >= r) { chosen = kv.Value.p; break; }
                    }
                    // Spawn offspring (possibly with mutation)
                    var child = chosen.SpawnOffspring(_rng, _cfg.MutationRate, _cfg.ParamMutationRate);
                    next.Add(child);
                }

                // Reassign stable IDs & reset strategies
                for (int i = 0; i < next.Count; i++)
                    next[i] = new Player(i, next[i].Strategy);

                pop = next;

                // Composition summary
                var composition = pop.GroupBy(p => p.Strategy.GetType().Name)
                    .Select(g => $"{g.Key}:{g.Count()}")
                    .OrderByDescending(s => int.Parse(s.Split(':')[1]))
                    .ToArray();
                Console.WriteLine("Population mix: " + string.Join(", ", composition));
            }

            return pop;
        }

        private static IReputationSystem BuildReputation(Config cfg) =>
            cfg.ReputationContext switch
            {
                ReputationContextType.None => new NoReputationSystem(),
                ReputationContextType.Public => new PublicReputationSystem(),
                ReputationContextType.Partial => new PartialReputationSystem(cfg.PartialReputationFanoutN),
                _ => new NoReputationSystem()
            };
    }

    #endregion

    #region Program Entrypoint

    public static class Program
    {
        public static void Main(string[] args)
        {
            var cfg = ArgParser.Parse(args);
            var rng = new Rng(cfg.Seed);

            // Build initial population from cfg.Players
            var players = new List<Player>();
            int idCounter = 0;
            foreach (var (name, count) in cfg.Players)
            {
                for (int i = 0; i < count; i++)
                {
                    var strat = StrategyFactory.CreateByName(name);
                    players.Add(new Player(idCounter++, strat));
                }
            }

            // Summary of setup
            Console.WriteLine("=== Prisoner's Dilemma Simulator ===");
            Console.WriteLine($"Mode={cfg.Mode}, Seed={cfg.Seed}, Players={players.Count}, RoundsPerMatch={cfg.RoundsPerMatch}");
            Console.WriteLine($"Horizon={cfg.Horizon}, L={cfg.FixedLengthL}, δ={cfg.Delta:F2}");
            Console.WriteLine($"Noise={cfg.NoiseType}, impl={cfg.ImplEps:F3} [{cfg.ImplMin:F3}-{cfg.ImplMax:F3}], obs={cfg.ObsEps:F3} [{cfg.ObsMin:F3}-{cfg.ObsMax:F3}]");
            Console.WriteLine($"AllowShift={cfg.AllowShift}, Reputation={cfg.ReputationContext} (PartialFanoutN={cfg.PartialReputationFanoutN})");
            Console.WriteLine($"Evolution: generations={cfg.Generations}, mutationRate={cfg.MutationRate:P1}, paramMutationRate={cfg.ParamMutationRate:P1}, baselineFitness={cfg.BaselineFitness:F2}");
            Console.WriteLine("Population spec: " + string.Join(", ", cfg.Players.Select(kv => $"{kv.Key}:{kv.Value}")));

            var tour = new Tournament(cfg, rng);

            if (cfg.Mode.Equals("evolve", StringComparison.OrdinalIgnoreCase))
            {
                tour.RunEvolution(players, Math.Max(1, cfg.Generations));
            }
            else
            {
                var rep = cfg.ReputationContext switch
                {
                    ReputationContextType.None => new NoReputationSystem(),
                    ReputationContextType.Public => new PublicReputationSystem(),
                    ReputationContextType.Partial => new PartialReputationSystem(cfg.PartialReputationFanoutN),
                    _ => new NoReputationSystem()
                };
                tour.RunSingleGeneration(players, rep);
            }

            // Hints for users
            Console.WriteLine("\nTIP: Try different presets:");
            Console.WriteLine("  - No noise, no reputation (clean reciprocity): noise=none reputation=none");
            Console.WriteLine("  - Moderate constant noise with forgiveness: noise=constant impl=0.02 obs=0.00 players=WSLSWithForgivenessContrition:24,GenerousTitForTat:24,ContriteTitForTat:24,AlwaysDefect:6,GrimTrigger:6");
            Console.WriteLine("  - Public reputation in messy worlds: reputation=public noise=variable implMin=0.00 implMax=0.05 obsMin=0.00 obsMax=0.03 allowShift=true");
            Console.WriteLine("  - Evolution toward robust strategies: mode=evolve generations=40 mutationRate=0.05 paramMutationRate=0.10");
        }
    }

    #endregion
}
